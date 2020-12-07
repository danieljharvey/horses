{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Interpreter
  ( spec,
  )
where

import qualified Data.Map as M
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.Swaps
import Test.Helpers
import Test.Hspec

testInterpret ::
  Scope () ->
  Expr Variable () ->
  Expr Variable () ->
  Expectation
testInterpret scope' expr' expected = do
  let result = interpret scope' mempty expr'
  result `shouldBe` Right expected

testInterpretFail ::
  Scope () ->
  Expr Variable () ->
  InterpreterError () ->
  Expectation
testInterpretFail scope' expr' expected = do
  let result = interpret scope' mempty expr'
  result `shouldBe` Left expected

interpret' ::
  Scope () ->
  Swaps ->
  Expr Variable () ->
  Either (InterpreterError ()) (Expr Variable ())
interpret' = interpret

spec :: Spec
spec =
  describe "Interpreter" $ do
    describe "Literals" $ do
      it "Booleans" $ do
        testInterpret
          mempty
          (bool True)
          (bool True)
        testInterpret
          mempty
          (bool False)
          (bool False)
      it "Integers" $ do
        testInterpret
          mempty
          (int (-100))
          (int (-100))
        testInterpret
          mempty
          (int 100)
          (int 100)
      it "Strings" $ do
        testInterpret
          mempty
          (str (StringType ""))
          (str (StringType ""))
        testInterpret
          mempty
          (str (StringType "poo"))
          (str (StringType "poo"))
    describe "Let and Var"
      $ it "let x = 1 in 1"
      $ do
        let f = MyLet mempty (named "x") (int 1) (MyVar mempty (named "x"))
        testInterpret mempty f (int 1)
    describe "Lambda and App" $ do
      it "let id = \\x -> x in (id 1)" $ do
        let f =
              MyLet
                mempty
                (named "id")
                (MyLambda mempty (named "x") (MyVar mempty (named "x")))
                (MyApp mempty (MyVar mempty (named "id")) (int 1))
        testInterpret mempty f (int 1)
      it "let const = \\a -> \\b -> a in const(1)" $ do
        let f =
              MyLet
                mempty
                (named "const")
                (MyLambda mempty (named "a") (MyLambda mempty (named "b") (MyVar mempty (named "a"))))
                (MyApp mempty (MyVar mempty (named "const")) (int 1))
        testInterpret mempty f $ MyLambda mempty (named "b") (MyVar mempty (NumberedVar 2))
      it "let const = \\a -> \\b -> a in ((const 1) 2)" $ do
        let f =
              MyLet
                mempty
                (named "const")
                (MyLambda mempty (tvFree 0) (MyLambda mempty (tvFree 1) (MyVar mempty (tvFree 0))))
                (MyApp mempty (MyApp mempty (MyVar mempty (named "const")) (int 1)) (int 2))
        testInterpret mempty f (int 1)
    describe "If" $ do
      it "Blows up when passed a non-bool" $ do
        let f = MyIf mempty (int 1) (bool True) (bool False)
        testInterpretFail mempty f (PredicateForIfMustBeABoolean (int 1))
      it "if True then 1 else 2" $ do
        let f = MyIf mempty (bool True) (int 1) (int 2)
        testInterpret mempty f (int 1)
      it "Destructures a pair" $ do
        let f =
              MyLet
                mempty
                (named "fst")
                ( MyLambda
                    mempty
                    (named "tuple")
                    ( MyLetPair
                        mempty
                        (named "a")
                        (named "b")
                        (MyVar mempty (named "tuple"))
                        (MyVar mempty (named "a"))
                    )
                )
                ( MyLet
                    mempty
                    (named "x")
                    (MyPair mempty (int 1) (int 2))
                    (MyApp mempty (MyVar mempty (named "fst")) (MyVar mempty (named "x")))
                )
        let result = interpret' mempty mempty f
        result `shouldBe` Right (int 1)
      it "Uses a higher order function twice without screwing the pooch" $ do
        let f =
              MyLet
                mempty
                (named "const2")
                (MyLambda mempty (named "a") (MyLambda mempty (named "b") (MyVar mempty (named "a"))))
                ( MyLet
                    mempty
                    (named "reuse")
                    ( MyRecord
                        mempty
                        ( M.fromList
                            [ ( mkName "first",
                                MyApp
                                  mempty
                                  (MyVar mempty (named "const2"))
                                  (MyLiteral mempty (MyInt 1))
                              ),
                              ( mkName "second",
                                MyApp
                                  mempty
                                  (MyVar mempty (named "const2"))
                                  (MyLiteral mempty (MyInt 2))
                              )
                            ]
                        )
                    )
                    ( MyApp
                        mempty
                        (MyRecordAccess mempty (MyVar mempty (named "reuse")) (mkName "first"))
                        (MyLiteral mempty (MyInt 100))
                    )
                )
        let result = interpret' mempty mempty f
        result `shouldBe` Right (int 1)
      it "Runs the internals of reduce function" $ do
        let reduceFunc =
              MyLet
                mempty
                (named "f")
                (MyLambda mempty (named "b") (MyLambda mempty (named "a") (str' "Horse")))
                ( MyApp
                    mempty
                    ( MyApp
                        mempty
                        (MyVar mempty (named "f"))
                        (str' "")
                    )
                    (MyLiteral mempty (MyInt 1))
                )
            scope' = mempty
        let result = interpret' scope' mempty reduceFunc
        result `shouldBe` Right (str' "Horse")
