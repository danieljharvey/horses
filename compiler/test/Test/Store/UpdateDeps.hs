{-# LANGUAGE OverloadedStrings #-}

module Test.Store.UpdateDeps
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
import qualified Data.Map as M
import Language.Mimsa.Store.UpdateDeps
import Language.Mimsa.Types.Identifiers ()
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "updateExprHash" $ do
    it "Empty makes empty" $ do
      let storeExpr = StoreExpression (bool True) mempty mempty
      updateExprHash storeExpr (ExprHash "1") (ExprHash "2") `shouldBe` mempty
    it "Match is swapped" $ do
      let storeExpr =
            StoreExpression
              (bool True)
              (M.singleton (Nothing, "dog") (ExprHash "1"))
              mempty
      updateExprHash storeExpr (ExprHash "1") (ExprHash "2")
        `shouldBe` M.singleton (Nothing, "dog") (ExprHash "2")

  describe
    "UpdateDeps"
    $ do
      it "Update with empty bindings is no-op" $ do
        let storeExpr = StoreExpression (bool True) mempty mempty
        updateStoreExpressionBindings testStdlib mempty storeExpr
          `shouldBe` Right storeExpr
      it "Replacing function with one that doesn't typecheck fails" $ do
        let storeExpr = getStoreExpression testStdlib (getHashOfName testStdlib "incrementInt")
        let newHash = getHashOfName testStdlib "id"
        let newBindings = M.singleton (Nothing, "addInt") newHash
        updateStoreExpressionBindings testStdlib newBindings storeExpr
          `shouldSatisfy` isLeft
      it "Replacing function with an equivalent one succeeds" $ do
        let storeExpr = getStoreExpression testStdlib (getHashOfName testStdlib "incrementInt")
        let newHash = getHashOfName testStdlib "subtractInt"
        let newBindings = M.singleton (Nothing, "addInt") newHash
        updateStoreExpressionBindings testStdlib newBindings storeExpr
          `shouldSatisfy` isRight
