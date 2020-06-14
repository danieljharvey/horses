{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Substitutor
  ( spec,
  )
where

-- import qualified Data.Map as M
import Language.Mimsa.Store.Substitutor (substitute)
import Language.Mimsa.Types
import Test.Hspec

trueStoreExpr :: StoreExpression
trueStoreExpr = StoreExpression mempty (MyBool True)

{-
falseStoreExpr :: StoreExpression
falseStoreExpr =
  StoreExpression
    (Bindings $ M.singleton (mkName "true") (ExprHash 1))
    (MyVar (mkName "true"))

storeWithTrueIn :: Store
storeWithTrueIn = Store (M.singleton (ExprHash 1) trueStoreExpr)
-}

spec :: Spec
spec = do
  describe "Substitutor" $ do
    describe "No deps, no problem" $ do
      it "Just unwraps everything" $ do
        substitute mempty trueStoreExpr `shouldBe` Right (mempty, trueStoreExpr, mempty)
{-    describe "One level of dep" $ do
it "Renames the dep to var0" $ do
  let hash = ExprHash 1
      expr = MyVar (Name "exciting")
      bindings' = Bindings $ M.singleton (Name "exciting") hash
      storeExpr = StoreExpression bindings' expr
      store' = Store (M.singleton hash trueStoreExpr)
  substitute store' storeExpr
    `shouldBe` Right
      ( [(Name "exciting", Name "var0")],
        StoreExpression
          (Bindings $ M.singleton (Name "var0") hash)
          (MyVar (Name "var0"))
      )-}
{-
describe "Combine two levels" $ do
  it "Combines trueStoreExpr and falseStoreExpr" $ do
     -}
