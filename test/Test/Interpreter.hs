{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Interpreter
  ( spec,
  )
where

import Data.Either (isLeft)
import Data.Text (Text)
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Types
import Test.Helpers
import Test.Hspec

testInterpret :: Scope -> Expr -> Expr -> Expectation
testInterpret scope' expr' expected = do
  result <- interpret scope' expr'
  result `shouldBe` (Right expected)

testInterpretFail :: Scope -> Expr -> Text -> Expectation
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
      it "let const = \\a -> \\b -> a in (const 1)" $ do
        let f =
              ( MyLet
                  (mkName "const")
                  (MyLambda (mkName "a") (MyLambda (mkName "b") (MyVar (Name "a"))))
                  (MyApp (MyVar (Name "const")) (int 1))
              )
        testInterpret mempty f $ MyLambda (Name "b") (MyVar (Name "a"))
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
        testInterpretFail mempty f "Predicate for If must be a Boolean"
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
        result `shouldSatisfy` \_ -> True
