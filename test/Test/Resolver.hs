{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Resolver
  ( spec,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Store.Resolver
import Language.Mimsa.Types
import Test.Helpers
import Test.Hspec

spec :: Spec
spec =
  describe "Resolver" $ do
    describe "extractVars" $ do
      it "Finds none where only literals" $ do
        extractVars (bool True) `shouldBe` mempty
        extractVars (int 1) `shouldBe` mempty
        extractVars (str (StringType "poo")) `shouldBe` mempty
      it "Finds a var" $
        extractVars (MyVar (Name "dog")) `shouldBe` S.singleton (Name "dog")
      it "Finds the vars in an if" $
        extractVars
          ( MyIf
              (MyVar (Name "one"))
              (MyVar (Name "two"))
              (MyVar (Name "three"))
          )
          `shouldBe` S.fromList [Name "one", Name "two", Name "three"]
      it "Does not include var introduced in Let" $
        extractVars
          ( MyLet
              (Name "newVar")
              (MyApp (MyVar (Name "keep")) (int 1))
              (MyVar (Name "newVar"))
          )
          `shouldBe` S.singleton (Name "keep")
      it "Does not introduce vars introduced in lambda" $
        extractVars (MyLambda (Name "newVar") (MyApp (MyVar (Name "keep")) (MyVar (Name "newVar"))))
          `shouldBe` S.singleton (Name "keep")
      it "Does not introduce built-ins" $
        extractVars (MyVar (Name "randomInt"))
          `shouldBe` S.empty
    describe "createStoreExpression" $ do
      it "Creates expressions from literals with empty Project" $ do
        createStoreExpression mempty (int 1)
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = mempty,
                  storeExpression = int 1,
                  storeTypeBindings = mempty
                }
            )
        createStoreExpression mempty (bool True)
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = mempty,
                  storeExpression = bool True,
                  storeTypeBindings = mempty
                }
            )
        createStoreExpression mempty (str (StringType "poo"))
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = mempty,
                  storeExpression = str (StringType "poo"),
                  storeTypeBindings = mempty
                }
            )
      it "Looks for vars and can't find them" $
        createStoreExpression mempty (MyVar (Name "missing"))
          `shouldBe` Left
            (MissingBinding (mkName "missing") mempty)
      it "Looks for vars and finds a built-in" $
        createStoreExpression mempty (MyVar (Name "randomInt"))
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = mempty,
                  storeExpression = MyVar (Name "randomInt"),
                  storeTypeBindings = mempty
                }
            )
      it "Looks for vars and finds them" $ do
        let hash = ExprHash 1234
            expr = MyVar (Name "missing")
            bindings' = Bindings $ M.singleton (Name "missing") hash
            storeExpr = createStoreExpression bindings' expr
        storeExpr
          `shouldBe` Right
            ( StoreExpression
                { storeBindings = Bindings $ M.singleton (Name "missing") hash,
                  storeExpression = expr,
                  storeTypeBindings = mempty
                }
            )
