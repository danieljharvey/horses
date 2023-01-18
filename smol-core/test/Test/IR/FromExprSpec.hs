{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.IR.FromExprSpec (spec) where

import Control.Monad.State
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Smol.Core.IR.FromExpr.Expr as IR
import Smol.Core.IR.FromResolvedExpr
import Smol.Core.IR.IRExpr
import Smol.Core.IR.ToLLVM.Patterns
import Smol.Core.Typecheck
import Smol.Core.Typecheck.FromParsedExpr
import Smol.Core.Types
import Smol.Core.Types.Expr
import Test.Helpers
import Test.Hspec

evalExpr :: Text -> IdentityExpr (Type Annotation)
evalExpr input =
  case elaborate (unsafeParseTypedExpr input $> mempty) of
    Right typedExpr -> fromResolvedExpr typedExpr
    Left e -> error (show e)

testEnv :: (Monoid ann) => IR.FromExprState ann
testEnv = IR.FromExprState mempty builtInTypes 1 mempty

getMainExpr :: Text -> IRExpr
getMainExpr = fst . createIR

createIR :: Text -> (IRExpr, [IRModulePart])
createIR input = do
  let smolExpr = evalExpr input
      (mainExpr, IR.FromExprState otherParts _ _ _) =
        runState (IR.fromExpr smolExpr) testEnv
   in (mainExpr, otherParts)

findFunction :: IRFunctionName -> [IRModulePart] -> IRFunction
findFunction fnName fns =
  let matches =
        mapMaybe
          ( \case
              IRFunctionDef f -> if irfName f == fnName then Just f else Nothing
              _ -> Nothing
          )
          fns
   in case matches of
        [a] -> a
        _ -> error $ "could not find " <> show fnName <> " in " <> show fns

spec :: Spec
spec = do
  describe "IR from expressions" $ do
    it "Prim ints become numbers" $ do
      getMainExpr "42" `shouldBe` IRPrim (IRPrimInt32 42)

    it "Creates a struct from a tuple" $ do
      let structType = IRStruct [IRInt32, IRInt32, IRInt32]
      getMainExpr "(1,2,3)"
        `shouldBe` IRInitialiseDataType
          (IRAlloc structType)
          structType
          structType
          [ IRSetTo [0] IRInt32 (IRPrim $ IRPrimInt32 1),
            IRSetTo [1] IRInt32 (IRPrim $ IRPrimInt32 2),
            IRSetTo [2] IRInt32 (IRPrim $ IRPrimInt32 3)
          ]

    it "Enum-like constructors become numbers" $ do
      getMainExpr "EQ" `shouldBe` IRPrim (IRPrimInt32 0)

    it "Constructors with arguments become structs" $ do
      let maybeIntType = IRStruct [IRInt32, IRArray 1 IRInt32]
          justIntType = IRStruct [IRInt32, IRInt32]
      getMainExpr "Just 42"
        `shouldBe` IRInitialiseDataType
          (IRAlloc maybeIntType)
          justIntType
          maybeIntType
          [ IRSetTo [0] IRInt32 (IRPrim $ IRPrimInt32 0),
            IRSetTo [1] IRInt32 (IRPrim $ IRPrimInt32 42)
          ]

    it "Non-enum constructors with no arguments still become structs" $ do
      let maybeIntType = IRStruct [IRInt32, IRArray 1 IRInt32]
          nothingIntType = IRStruct [IRInt32]
      getMainExpr "(Nothing : Maybe Int)"
        `shouldBe` IRInitialiseDataType
          (IRAlloc maybeIntType)
          nothingIntType
          maybeIntType
          [ IRSetTo [0] IRInt32 (IRPrim $ IRPrimInt32 1)
          ]

    -- ignore what the function DOES, what does it give us back to play with?
    -- this has an empty env, in future lets optimise so that we don't bother
    -- with the struct etc for these cases, and instead just return a function
    -- pointer
    it "Lambda returns closure" $ do
      let functionPointerType = IRFunctionType [IRInt32, IRStruct []] IRInt32
          closureType =
            IRStruct
              [ IRPointer functionPointerType,
                IRStruct []
              ]
      getMainExpr "(\\a -> 1 : Nat -> Nat)"
        `shouldBe` IRInitialiseDataType
          (IRAlloc closureType)
          closureType
          closureType
          [ IRSetTo [0] (IRPointer functionPointerType) (IRFuncPointer "function1")
          ]

    -- ignore what the function DOES, what does it give us back to play with?
    -- this has an empty env, in future lets optimise so that we don't bother
    -- with the struct etc for these cases, and instead just return a function
    -- pointer
    it "Runs lambda from closure" $ do
      let functionPointerType = IRFunctionType [IRInt32, IRStruct []] IRInt32
          closureType =
            IRStruct
              [ IRPointer functionPointerType,
                IRStruct []
              ]
          innerStructType =
            IRStruct
              [IRPointer (IRFunctionType [IRInt32, IRStruct []] IRInt32), IRStruct []]

      getMainExpr "(\\a -> 1 : Nat -> Nat) 2"
        `shouldBe` IRLet
          "closure2"
          ( IRInitialiseDataType
              (IRAlloc closureType)
              closureType
              closureType
              [ IRSetTo [0] (IRPointer functionPointerType) (IRFuncPointer "function1")
              ]
          )
          ( IRApply
              (IRStruct [IRPointer (IRFunctionType [IRInt32, IRStruct []] IRInt32), IRStruct []])
              ( IRStructPath
                  [0]
                  ( IRInitialiseDataType
                      (IRAlloc innerStructType)
                      innerStructType
                      innerStructType
                      [ IRSetTo
                          [0]
                          (IRPointer (IRFunctionType [IRInt32, IRStruct []] IRInt32))
                          (IRFuncPointer "function1")
                      ]
                  )
              )
              [IRPrim (IRPrimInt32 2), IRPointerTo [1] (IRVar "closure2")]
          )

    xit "Runs a function returned from a lambda twice" $ do
      let func2Env = IRStruct [IRInt32]
          func2Type = IRFunctionType [IRInt32, IRPointer func2Env] IRInt32
          add1ReturnType =
            IRStruct
              [ IRPointer func2Type,
                func2Env
              ]

      getMainExpr "(\\a -> \\b -> a + b : Nat -> Nat -> Nat) 1 2"
        `shouldBe` IRLet
          "emptyenv"
          (IRAlloc (IRStruct []))
          ( IRLet
              "closure"
              ( IRApply
                  (IRFunctionType [IRInt32] add1ReturnType)
                  (IRFuncPointer "add1")
                  [IRPrim (IRPrimInt32 20), IRVar "emptyenv"]
              )
              ( IRApply
                  func2Type
                  (IRStructPath [0] (IRVar "closure"))
                  [ IRPrim (IRPrimInt32 22),
                    IRPointerTo [1] (IRVar "closure")
                  ]
              )
          )

    it "Creates function that returns a lambda closure" $ do
      let (_expr, fns) = createIR "(\\a -> \\b -> a + b : Nat -> Nat -> Nat) 1 2"
      let func2Env = IRStruct [IRInt32]
          func2Type =
            IRFunctionType
              [IRInt32, func2Env]
              IRInt32
          add1ReturnType =
            IRStruct
              [ IRPointer func2Type,
                func2Env
              ]
      -- this is the \\a -> ... function that returns a closure with env
      findFunction "function1" fns
        `shouldBe` IRFunction
          { irfName = "function1",
            irfArgs = [(IRInt32, "a"), (IRStruct [], "env")],
            irfReturn =
              IRStruct
                [ IRPointer (IRFunctionType [IRInt32, IRStruct [IRInt32]] IRInt32),
                  IRStruct [IRInt32]
                ],
            irfBody =
              [ IRRet
                  add1ReturnType
                  ( IRInitialiseDataType
                      (IRAlloc add1ReturnType)
                      add1ReturnType
                      add1ReturnType
                      [ IRSetTo
                          [0]
                          (IRPointer func2Type)
                          (IRFuncPointer "function2"),
                        IRSetTo
                          [1, 0]
                          IRInt32
                          (IRVar "a")
                      ]
                  )
              ]
          }

    it "Creates lambda closure that returns a plain value" $ do
      let (_expr, fns) = createIR "(\\a -> \\b -> a + b : Nat -> Nat -> Nat) 1 2"
      findFunction "function2" fns
        `shouldBe` IRFunction
          { irfName = "function2",
            irfArgs = [(IRInt32, "b"), (IRStruct [IRInt32], "env")],
            irfReturn = IRInt32,
            irfBody =
              [ IRRet
                  IRInt32
                  (IRInfix IRAdd (IRStructPath [0] (IRVar "env")) (IRVar "b"))
              ]
          }

    it "Creates constructor with arg" $ do
      getMainExpr "Just 41"
        `shouldBe` IRInitialiseDataType
          (IRAlloc (IRStruct [IRInt32, IRArray 1 IRInt32]))
          (IRStruct [IRInt32, IRInt32])
          (IRStruct [IRInt32, IRArray 1 IRInt32])
          [ IRSetTo {irstPath = [0], irstType = IRInt32, irstExpr = IRPrim (IRPrimInt32 0)},
            IRSetTo
              { irstPath = [1],
                irstType =
                  IRInt32,
                irstExpr = IRPrim (IRPrimInt32 41)
              }
          ]

    it "Pattern matches enum" $ do
      getMainExpr "(case LT of GT -> 21 | EQ -> 23 | LT -> 42 : Nat)"
        `shouldBe` IRMatch
          (IRPrim (IRPrimInt32 2))
          IRInt32
          ( NE.fromList
              [ IRMatchCase
                  { irmcType = IRInt32,
                    irmcPatternPredicate = [PathEquals ValuePath (IRPrimInt32 1)],
                    irmcGetPath = mempty,
                    irmcExpr = IRPrim (IRPrimInt32 21)
                  },
                IRMatchCase
                  { irmcType = IRInt32,
                    irmcPatternPredicate = [PathEquals ValuePath (IRPrimInt32 0)],
                    irmcGetPath = mempty,
                    irmcExpr = IRPrim (IRPrimInt32 23)
                  },
                IRMatchCase
                  { irmcType = IRInt32,
                    irmcPatternPredicate = [PathEquals ValuePath (IRPrimInt32 2)],
                    irmcGetPath = mempty,
                    irmcExpr = IRPrim (IRPrimInt32 42)
                  }
              ]
          )

    it "Pattern matches 2-arg type" $ do
      let typeTheseNatNat = IRStruct [IRInt32, IRArray 2 IRInt32]
          thisNatNat = IRStruct [IRInt32, IRInt32]
          thatNatNat = IRStruct [IRInt32, IRInt32]
          theseNatNat = IRStruct [IRInt32, IRInt32, IRInt32]
      getMainExpr "(case (This 42 : These Nat Nat) of This a -> a | That b -> 0 | These tA tB -> tA + tB : Nat)"
        `shouldBe` IRMatch
          ( IRInitialiseDataType
              (IRAlloc typeTheseNatNat)
              thisNatNat
              typeTheseNatNat
              [ IRSetTo [0] IRInt32 (IRPrim (IRPrimInt32 2)),
                IRSetTo [1] IRInt32 (IRPrim (IRPrimInt32 42))
              ]
          )
          IRInt32
          ( NE.fromList
              [ IRMatchCase
                  { irmcType = thisNatNat,
                    irmcPatternPredicate =
                      [ PathEquals
                          (StructPath $ NE.fromList [0])
                          (IRPrimInt32 2)
                      ],
                    irmcGetPath = M.singleton "a" (StructPath $ NE.fromList [1]),
                    irmcExpr = IRVar "a"
                  },
                IRMatchCase
                  { irmcType = thatNatNat,
                    irmcPatternPredicate =
                      [ PathEquals
                          (StructPath $ NE.fromList [0])
                          (IRPrimInt32 0)
                      ],
                    irmcGetPath = M.singleton "b" (StructPath $ NE.fromList [1]),
                    irmcExpr = IRPrim (IRPrimInt32 0)
                  },
                IRMatchCase
                  { irmcType = theseNatNat,
                    irmcPatternPredicate =
                      [ PathEquals
                          (StructPath $ NE.fromList [0])
                          (IRPrimInt32 1)
                      ],
                    irmcGetPath =
                      M.fromList
                        [ ("tA", StructPath $ NE.fromList [1]),
                          ("tB", StructPath $ NE.fromList [2])
                        ],
                    irmcExpr = IRInfix IRAdd (IRVar "tA") (IRVar "tB")
                  }
              ]
          )
