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

testInterpret :: Scope -> Expr Name -> Expr Name -> Expectation
testInterpret scope' expr' expected = do
  result <- interpret scope' expr'
  result `shouldBe` (Right expected)

testInterpretFail :: Scope -> Expr Name -> InterpreterError -> Expectation
testInterpretFail scope' expr' expected = do
  result <- interpret scope' expr'
  result `shouldBe` (Left expected)

spec :: Spec
spec = do
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
    describe "Let and Var" $ do
      it "let x = 1 in 1" $ do
        let f = (MyLet (mkName "x") (int 1) (MyVar (mkName "x")))
        testInterpret mempty f (int 1)
    describe "Lambda and App" $ do
      it "let id = \\x -> x in (id 1)" $ do
        let f =
              ( MyLet
                  (mkName "id")
                  (MyLambda (mkName "x") (MyVar (mkName "x")))
                  (MyApp (MyVar (mkName "id")) (int 1))
              )
        testInterpret mempty f (int 1)
      it "let const = \\a -> \\b -> a in const(1)" $ do
        let f =
              ( MyLet
                  (mkName "const")
                  (MyLambda (mkName "a") (MyLambda (mkName "b") (MyVar (Name "a"))))
                  (MyApp (MyVar (Name "const")) (int 1))
              )
        testInterpret mempty f $ MyLambda (Name "b") (MyVar (Name "var1"))
      it "let const = \\a -> \\b -> a in ((const 1) 2)" $ do
        let f =
              ( MyLet
                  (mkName "const")
                  (MyLambda (mkName "a") (MyLambda (mkName "b") (MyVar (Name "a"))))
                  (MyApp (MyApp (MyVar (Name "const")) (int 1)) (int 2))
              )
        testInterpret mempty f (int 1)
    describe "If" $ do
      it "Blows up when passed a non-bool" $ do
        let f = (MyIf (int 1) (bool True) (bool False))
        testInterpretFail mempty f (PredicateForIfMustBeABoolean (int 1))
      it "if True then 1 else 2" $ do
        let f = (MyIf (bool True) (int 1) (int 2))
        testInterpret mempty f (int 1)
    describe "BuiltIns" $ do
      it "Can't find stupidMadeUpFunction" $ do
        let f = MyVar (Name "stupidMadeUpFunction")
        result <- interpret mempty f
        result `shouldSatisfy` isLeft
      it "Finds and uses randomInt" $ do
        let f = MyVar (Name "randomInt")
            scope' = mempty
        result <- interpret scope' f
        print result
        result `shouldSatisfy` \(Right (MyLiteral (MyInt _))) -> True
      it "Finds and uses randomIntFrom" $ do
        let f = MyApp (MyVar (Name "randomIntFrom")) (int 10)
            scope' = mempty
        result <- interpret scope' f
        print result
        result `shouldSatisfy` (\(Right (MyLiteral (MyInt i))) -> i > 9)
      it "Uses a two arg function inside an If" $ do
        let f =
              MyIf
                ( MyApp
                    (MyApp (MyVar (Name "eqInt")) (int 1))
                    (int 2)
                )
                (int 1)
                (int 0)
            scope' = mempty
        result <- interpret scope' f
        result `shouldBe` Right (int 0)
      it "Destructures a pair" $ do
        let f =
              ( MyLet
                  (mkName "fst")
                  ( MyLambda
                      (mkName "tuple")
                      ( MyLetPair
                          (mkName "a")
                          (mkName "b")
                          (MyVar (mkName "tuple"))
                          (MyVar (mkName "a"))
                      )
                  )
                  ( MyLet
                      (mkName "x")
                      (MyPair (int 1) (int 2))
                      (MyApp (MyVar (mkName "fst")) (MyVar (mkName "x")))
                  )
              )
        result <- interpret mempty f
        result `shouldBe` Right (int 1)
      it "Uses a higher order function twice without screwing the pooch" $ do
        let f =
              MyLet
                (mkName "const2")
                (MyLambda (mkName "a") (MyLambda (mkName "b") (MyVar (mkName "a"))))
                ( MyLet
                    (mkName "reuse")
                    ( MyRecord
                        ( M.fromList
                            [ ( mkName "first",
                                MyApp
                                  (MyVar (mkName "const2"))
                                  (MyLiteral (MyInt 1))
                              ),
                              ( mkName "second",
                                MyApp
                                  (MyVar (mkName "const2"))
                                  (MyLiteral (MyInt 2))
                              )
                            ]
                        )
                    )
                    ( MyApp
                        (MyRecordAccess (MyVar (mkName "reuse")) (mkName "first"))
                        (MyLiteral (MyInt 100))
                    )
                )
        result <- interpret mempty f
        result `shouldBe` Right (int 1)
      it "Uses var names in lambdas that conflict with the ones inside our built-in function without breaking" $ do
        let ifFunc =
              MyLambda
                (Name "x")
                ( MyLambda
                    (Name "y")
                    ( MyIf
                        ( MyApp
                            ( MyApp
                                (MyVar (Name "eqInt"))
                                (MyVar (Name "x"))
                            )
                            (MyVar (Name "y"))
                        )
                        (int 1)
                        (int 0)
                    )
                )
            f = MyApp (MyApp ifFunc (int 1)) (int 2)
            scope' = mempty
        result <- interpret scope' f
        result `shouldBe` Right (int 0)
