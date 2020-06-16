{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Interpreter
  ( spec,
  )
where

import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Types
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = do
  describe "Interpreter" $ do
    describe "Literals" $ do
      it "Booleans" $ do
        interpret mempty (bool True) `shouldBe` Right (bool True)
        interpret mempty (bool False) `shouldBe` Right (bool False)
      it "Integers" $ do
        interpret mempty (int (-100)) `shouldBe` Right (int (-100))
        interpret mempty (int 100) `shouldBe` Right (int 100)
      it "Strings" $ do
        interpret mempty (str (StringType "")) `shouldBe` Right (str (StringType ""))
        interpret mempty (str (StringType "poo")) `shouldBe` Right (str (StringType "poo"))
    describe "Let and Var" $ do
      it "let x = 1 in 1" $ do
        let f = (MyLet (mkName "x") (int 1) (MyVar (mkName "x")))
        interpret mempty f `shouldBe` Right (int 1)
    describe "Lambda and App" $ do
      it "let id = \\x -> x in (id 1)" $ do
        let f =
              ( MyLet
                  (mkName "id")
                  (MyLambda (mkName "x") (MyVar (mkName "x")))
                  (MyApp (MyVar (mkName "id")) (int 1))
              )
        interpret mempty f `shouldBe` Right (int 1)
      it "let const = \\a -> \\b -> a in (const 1)" $ do
        let f =
              ( MyLet
                  (mkName "const")
                  (MyLambda (mkName "a") (MyLambda (mkName "b") (MyVar (Name "a"))))
                  (MyApp (MyVar (Name "const")) (int 1))
              )
        interpret mempty f `shouldBe` Right (MyLambda (Name "b") (MyVar (Name "a")))
      it "let const = \\a -> \\b -> a in ((const 1) 2)" $ do
        let f =
              ( MyLet
                  (mkName "const")
                  (MyLambda (mkName "a") (MyLambda (mkName "b") (MyVar (Name "a"))))
                  (MyApp (MyApp (MyVar (Name "const")) (int 1)) (int 2))
              )
        interpret mempty f `shouldBe` Right (int 1)
    describe "If" $ do
      it "Blows up when passed a non-bool" $ do
        let f = (MyIf (int 1) (bool True) (bool False))
        interpret mempty f `shouldBe` Left "Predicate for If must be a Boolean"
      it "if True then 1 else 2" $ do
        let f = (MyIf (bool True) (int 1) (int 2))
        interpret mempty f `shouldBe` Right (int 1)
