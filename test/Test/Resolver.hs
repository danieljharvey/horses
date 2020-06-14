{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Resolver
  ( spec,
  )
where

import qualified Data.Set as S
import Language.Mimsa.Store.Resolver
import Language.Mimsa.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "Resolver" $ do
    describe "extractVars" $ do
      it "Finds none where only literals" $ do
        extractVars (MyBool True) `shouldBe` mempty
        extractVars (MyInt 1) `shouldBe` mempty
        extractVars (MyString (StringType "poo")) `shouldBe` mempty
      it "Finds a var" $ do
        extractVars (MyVar (Name "dog")) `shouldBe` S.singleton (Name "dog")
      it "Finds the vars in an if" $ do
        extractVars
          ( MyIf
              (MyVar (Name "one"))
              (MyVar (Name "two"))
              (MyVar (Name "three"))
          )
          `shouldBe` S.fromList [Name "one", Name "two", Name "three"]
      it "Does not include var introduced in Let" $ do
        extractVars
          ( MyLet
              (Name "newVar")
              (MyApp (MyVar (Name "keep")) (MyInt 1))
              (MyVar (Name "newVar"))
          )
          `shouldBe` S.singleton (Name "keep")
      it "Does not introduce vars introduced in lambda" $ do
        extractVars (MyLambda (Name "newVar") (MyApp (MyVar (Name "keep")) (MyVar (Name "newVar"))))
          `shouldBe` S.singleton (Name "keep")
    describe "createStoreExpression" $ do
      it "Creates expressions from literals with empty StoreEnv" $ do
        createStoreExpression mempty (MyInt 1)
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = mempty,
                  storeExpression = MyInt 1
                }
            )
        createStoreExpression mempty (MyBool True)
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = mempty,
                  storeExpression = MyBool True
                }
            )
        createStoreExpression mempty (MyString (StringType "poo"))
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = mempty,
                  storeExpression = MyString (StringType "poo")
                }
            )
