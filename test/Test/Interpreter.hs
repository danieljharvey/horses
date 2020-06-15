{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Interpreter
  ( spec,
  )
where

import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "Interpreter" $ do
    describe "Literals" $ do
      it "Booleans" $ do
        interpret mempty (MyBool True) `shouldBe` Right (MyBool True)
        interpret mempty (MyBool False) `shouldBe` Right (MyBool False)
      it "Integers" $ do
        interpret mempty (MyInt (-100)) `shouldBe` Right (MyInt (-100))
        interpret mempty (MyInt 100) `shouldBe` Right (MyInt 100)
      it "Strings" $ do
        interpret mempty (MyString (StringType "")) `shouldBe` Right (MyString (StringType ""))
        interpret mempty (MyString (StringType "poo")) `shouldBe` Right (MyString (StringType "poo"))
    describe "Let and Var" $ do
      it "let x = 1 in 1" $ do
        let f = (MyLet (mkName "x") (MyInt 1) (MyVar (mkName "x")))
        interpret mempty f `shouldBe` Right (MyInt 1)
    describe "Lambda and App" $ do
      it "let id = \\x -> x in (id 1)" $ do
        let f =
              ( MyLet
                  (mkName "id")
                  (MyLambda (mkName "x") (MyVar (mkName "x")))
                  (MyApp (MyVar (mkName "id")) (MyInt 1))
              )
        interpret mempty f `shouldBe` Right (MyInt 1)
      it "let const = \\a -> \b -> a in ((const 1) 2)" $ do
        let f =
              ( MyLet
                  (mkName "const")
                  (MyLambda (mkName "a") (MyLambda (mkName "b") (MyVar (Name "a"))))
                  (MyApp (MyApp (MyVar (Name "const")) (MyInt 1)) (MyInt 2))
              )
        interpret mempty f `shouldBe` Right (MyInt 1)
    describe "If" $ do
      it "Blows up when passed a non-bool" $ do
        let f = (MyIf (MyInt 1) (MyBool True) (MyBool False))
        interpret mempty f `shouldBe` Left "Predicate for If must be a Boolean"
      it "if True then 1 else 2" $ do
        let f = (MyIf (MyBool True) (MyInt 1) (MyInt 2))
        interpret mempty f `shouldBe` Right (MyInt 1)
