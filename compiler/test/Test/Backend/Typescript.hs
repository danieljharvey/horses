{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.Typescript
  ( spec,
  )
where

import qualified Data.Map as M
import Language.Mimsa.Backend.Typescript.FromExpr
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Typechecker
import Test.Hspec

spec :: Spec
spec = do
  fdescribe "Typescript" $ do
    describe "pretty print Typescript AST" $ do
      it "literals" $ do
        prettyPrint (TSBool True) `shouldBe` "true"
        prettyPrint (TSInt 100) `shouldBe` "100"
        prettyPrint (TSString "egg") `shouldBe` "\"egg\""
      it "function" $ do
        prettyPrint
          ( TSFunction
              "a"
              (TSType "boolean" [])
              (TSFunctionBody (TSBody mempty (TSLit (TSInt 1))))
          )
          `shouldBe` "(a: boolean) => 1"
        prettyPrint
          ( TSFunction
              "maybeA"
              (TSType "Maybe" [TSTypeVar "A"])
              (TSFunctionBody (TSBody mempty (TSLit (TSInt 1))))
          )
          `shouldBe` "(maybeA: Maybe<A>) => 1"
        prettyPrint
          ( TSFunction
              "maybeA"
              (TSType "Maybe" [TSTypeVar "A"])
              ( TSFunctionBody
                  ( TSBody
                      [ TSAssignment (TSPatternVar "b") (TSLetBody (TSBody [] (TSLit (TSBool True))))
                      ]
                      (TSLit (TSInt 1))
                  )
              )
          )
          `shouldBe` "(maybeA: Maybe<A>) => { const b = true; return 1 }"
      it "function application" $ do
        prettyPrint (TSApp (TSVar "id") (TSLit (TSBool True)))
          `shouldBe` "id(true)"
        prettyPrint (TSApp (TSApp (TSVar "id") (TSLit (TSBool True))) (TSLit (TSInt 1)))
          `shouldBe` "id(true)(1)"
      it "infix operators" $ do
        prettyPrint (TSInfix TSEquals (TSLit (TSInt 1)) (TSLit (TSInt 2)))
          `shouldBe` "1 === 2"
      it "record" $ do
        prettyPrint
          ( TSRecord
              ( M.fromList
                  [ ( "a",
                      TSLit (TSInt 1)
                    ),
                    ("b", TSLit (TSBool True))
                  ]
              )
          )
          `shouldBe` "{ a: 1, b: true }"
      it "record access" $ do
        prettyPrint (TSRecordAccess "a" (TSVar "record")) `shouldBe` "record.a"
      it "array" $ do
        prettyPrint (TSArray [TSLit (TSInt 1), TSLit (TSInt 2), TSLit (TSInt 3)])
          `shouldBe` "[1,2,3]"
      it "array access" $ do
        prettyPrint (TSArrayAccess 2 (TSVar "array"))
          `shouldBe` "array[2]"
      it "ternary" $ do
        prettyPrint
          ( TSTernary
              (TSLit (TSBool True))
              (TSLit (TSInt 1))
              (TSLit (TSInt 2))
          )
          `shouldBe` "true ? 1 : 2"
      it "patterns" $ do
        prettyPrint (TSPatternVar "a") `shouldBe` "a"
        prettyPrint TSPatternWildcard `shouldBe` "_"
        prettyPrint
          ( TSPatternPair
              (TSPatternVar "a")
              (TSPatternVar "b")
          )
          `shouldBe` "[a,b]"
        prettyPrint
          ( TSPatternRecord
              ( M.fromList
                  [("a", TSPatternVar "a"), ("b", TSPatternVar "b")]
              )
          )
          `shouldBe` "{ a: a, b: b }"
        prettyPrint (TSPatternConstructor "Just" [TSPatternVar "a"])
          `shouldBe` "{ vars: [a] }"
      it "top level module" $ do
        prettyPrint (TSModule (TSBody mempty (TSLit (TSBool True))))
          `shouldBe` "export const main = true"
        prettyPrint
          ( TSModule
              ( TSBody
                  [ TSAssignment
                      (TSPatternVar "a")
                      (TSLetBody (TSBody mempty (TSLit (TSBool True))))
                  ]
                  (TSVar "a")
              )
          )
          `shouldBe` "const a = true\nexport const main = a"
    describe "from typed expression" $ do
      let mtBool = MTPrim mempty MTBool
      it "const bool" $ do
        fromExpr (MyLiteral (mtBool, mempty) (MyBool True))
          `shouldBe` TSModule (TSBody [] (TSLit (TSBool True)))
      it "let a = true in a" $
        do
          fromExpr
            ( MyLet
                (mtBool, mempty)
                "a"
                ( MyLiteral (mtBool, mempty) (MyBool True)
                )
                (MyVar (mtBool, mempty) "a")
            )
            `shouldBe` TSModule
              ( TSBody
                  [ TSAssignment
                      (TSPatternVar "a")
                      (TSLetBody (TSBody mempty (TSLit (TSBool True))))
                  ]
                  (TSVar "a")
              )
      it "let (a,b) = (true,false) in a" $ do
        fromExpr
          ( MyLetPattern
              (MTPair mempty mtBool mtBool, mempty)
              (PPair (MTPair mempty mtBool mtBool, mempty) (PVar (mtBool, mempty) "a") (PVar (mtBool, mempty) "b"))
              ( MyPair
                  (MTPair mempty mtBool mtBool, mempty)
                  (MyLiteral (mtBool, mempty) (MyBool True))
                  (MyLiteral (mtBool, mempty) (MyBool False))
              )
              (MyVar (mtBool, mempty) "a")
          )
          `shouldBe` TSModule
            ( TSBody
                [ TSAssignment
                    (TSPatternPair (TSPatternVar "a") (TSPatternVar "b"))
                    (TSLetBody (TSBody mempty (TSArray [TSLit (TSBool True), TSLit (TSBool False)])))
                ]
                (TSVar "a")
            )
