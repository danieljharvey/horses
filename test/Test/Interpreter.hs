{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Interpreter
  ( spec,
  )
where

import Data.Either (isLeft)
import qualified Data.Map as M
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Types
import Test.Helpers
import Test.Hspec

testInterpret :: Scope -> Expr Variable -> Expr Variable -> Expectation
testInterpret scope' expr' expected = do
  result <- interpret scope' expr'
  result `shouldBe` Right expected

testInterpretFail :: Scope -> Expr Variable -> InterpreterError -> Expectation
testInterpretFail scope' expr' expected = do
  result <- interpret scope' expr'
  result `shouldBe` Left expected

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
        let f = MyLet (named "x") (int 1) (MyVar (named "x"))
        testInterpret mempty f (int 1)
    describe "Lambda and App" $ do
      it "let id = \\x -> x in (id 1)" $ do
        let f =
              MyLet
                (named "id")
                (MyLambda (named "x") (MyVar (named "x")))
                (MyApp (MyVar (named "id")) (int 1))
        testInterpret mempty f (int 1)
      it "let const = \\a -> \\b -> a in const(1)" $ do
        let f =
              MyLet
                (named "const")
                (MyLambda (named "a") (MyLambda (named "b") (MyVar (named "a"))))
                (MyApp (MyVar (named "const")) (int 1))
        testInterpret mempty f $ MyLambda (named "b") (MyVar (NumberedVar 1))
      it "let const = \\a -> \\b -> a in ((const 1) 2)" $ do
        let f =
              MyLet
                (named "const")
                (MyLambda (named "a") (MyLambda (named "b") (MyVar (named "a"))))
                (MyApp (MyApp (MyVar (named "const")) (int 1)) (int 2))
        testInterpret mempty f (int 1)
    describe "If" $ do
      it "Blows up when passed a non-bool" $ do
        let f = MyIf (int 1) (bool True) (bool False)
        testInterpretFail mempty f (PredicateForIfMustBeABoolean (int 1))
      it "if True then 1 else 2" $ do
        let f = MyIf (bool True) (int 1) (int 2)
        testInterpret mempty f (int 1)
    describe "BuiltIns" $ do
      it "Can't find stupidMadeUpFunction" $ do
        let f = MyVar (named "stupidMadeUpFunction")
        result <- interpret mempty f
        result `shouldSatisfy` isLeft
      it "Finds and uses randomInt" $ do
        let f = MyVar (builtIn "randomInt")
            scope' = mempty
        result <- interpret scope' f
        print result
        result `shouldSatisfy` \(Right (MyLiteral (MyInt _))) -> True
      it "Finds and uses randomIntFrom" $ do
        let f = MyApp (MyVar (builtIn "randomIntFrom")) (int 10)
            scope' = mempty
        result <- interpret scope' f
        print result
        result `shouldSatisfy` (\(Right (MyLiteral (MyInt i))) -> i > 9)
      it "Uses a two arg function inside an If" $ do
        let f =
              MyIf
                ( MyApp
                    (MyApp (MyVar (builtIn "eqInt")) (int 1))
                    (int 2)
                )
                (int 1)
                (int 0)
            scope' = mempty
        result <- interpret scope' f
        result `shouldBe` Right (int 0)
      it "Destructures a pair" $ do
        let f =
              MyLet
                (named "fst")
                ( MyLambda
                    (named "tuple")
                    ( MyLetPair
                        (named "a")
                        (named "b")
                        (MyVar (named "tuple"))
                        (MyVar (named "a"))
                    )
                )
                ( MyLet
                    (named "x")
                    (MyPair (int 1) (int 2))
                    (MyApp (MyVar (named "fst")) (MyVar (named "x")))
                )
        result <- interpret mempty f
        result `shouldBe` Right (int 1)
      it "Uses a higher order function twice without screwing the pooch" $ do
        let f =
              MyLet
                (named "const2")
                (MyLambda (named "a") (MyLambda (named "b") (MyVar (named "a"))))
                ( MyLet
                    (named "reuse")
                    ( MyRecord
                        ( M.fromList
                            [ ( mkName "first",
                                MyApp
                                  (MyVar (named "const2"))
                                  (MyLiteral (MyInt 1))
                              ),
                              ( mkName "second",
                                MyApp
                                  (MyVar (named "const2"))
                                  (MyLiteral (MyInt 2))
                              )
                            ]
                        )
                    )
                    ( MyApp
                        (MyRecordAccess (MyVar (named "reuse")) (mkName "first"))
                        (MyLiteral (MyInt 100))
                    )
                )
        result <- interpret mempty f
        result `shouldBe` Right (int 1)
      it "Uses var names in lambdas that conflict with the ones inside our built-in function without breaking" $ do
        let ifFunc =
              MyLambda
                (named "x")
                ( MyLambda
                    (named "y")
                    ( MyIf
                        ( MyApp
                            ( MyApp
                                (MyVar (builtIn "eqInt"))
                                (MyVar (named "x"))
                            )
                            (MyVar (named "y"))
                        )
                        (int 1)
                        (int 0)
                    )
                )
            f = MyApp (MyApp ifFunc (int 1)) (int 2)
            scope' = mempty
        result <- interpret scope' f
        result `shouldBe` Right (int 0)
      it "Runs the internals of reduce function" $ do
        let reduceFunc =
              MyLet
                (named "f")
                (MyLambda (named "b") (MyLambda (named "a") (str' "Horse")))
                ( MyApp
                    ( MyApp
                        (MyVar (named "f"))
                        (str' "")
                    )
                    (MyLiteral (MyInt 1))
                )
            scope' = mempty
        result <- interpret scope' reduceFunc
        result `shouldBe` Right (str' "Horse")
