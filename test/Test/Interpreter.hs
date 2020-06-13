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
        interpret (MyBool True) `shouldBe` Right (MyBool True)
        interpret (MyBool False) `shouldBe` Right (MyBool False)
      it "Integers" $ do
        interpret (MyInt (-100)) `shouldBe` Right (MyInt (-100))
        interpret (MyInt 100) `shouldBe` Right (MyInt 100)
      it "Strings" $ do
        interpret (MyString (StringType "")) `shouldBe` Right (MyString (StringType ""))
        interpret (MyString (StringType "poo")) `shouldBe` Right (MyString (StringType "poo"))
    describe "Let and Var" $ do
      it "let x = 1 in 1" $ do
        let f = (MyLet (mkName "x") (MyInt 1) (MyVar (mkName "x")))
        interpret f `shouldBe` Right (MyInt 1)
    describe "Lambda and App" $ do
      it "let id = \\x -> x in (id 1)" $ do
        let f =
              ( MyLet
                  (mkName "id")
                  (MyLambda (mkName "x") (MyVar (mkName "x")))
                  (MyApp (MyVar (mkName "id")) (MyInt 1))
              )
        interpret f `shouldBe` Right (MyInt 1)
    describe "If" $ do
      it "Blows up when passed a non-bool" $ do
        let f = (MyIf (MyInt 1) (MyBool True) (MyBool False))
        interpret f `shouldBe` Left "Predicate for If must be a Boolean"
      it "if True then 1 else 2" $ do
        let f = (MyIf (MyBool True) (MyInt 1) (MyInt 2))
        interpret f `shouldBe` Right (MyInt 1)
