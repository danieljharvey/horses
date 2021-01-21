{-# LANGUAGE OverloadedStrings #-}

module Test.UpdateDeps
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
import qualified Data.Map as M
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store.UpdateDeps
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

getHashOfName :: Project ann -> Name -> ExprHash
getHashOfName prj name =
  case lookupBindingName prj name of
    Just a -> a
    _ -> error "could not getHashOfName"

getStoreExpression :: Project ann -> ExprHash -> StoreExpression ann
getStoreExpression project exprHash' =
  case lookupExprHash project exprHash' of
    Just a -> a
    _ -> error "could not getStoreExpression"

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
              (Bindings $ M.singleton (mkName "dog") (ExprHash "1"))
              mempty
      updateExprHash storeExpr (ExprHash "1") (ExprHash "2")
        `shouldBe` Bindings
          ( M.singleton (mkName "dog") (ExprHash "2")
          )

  describe
    "UpdateDeps"
    $ do
      it "Update with empty bindings is no-op" $ do
        let storeExpr = StoreExpression (bool True) mempty mempty
        updateStoreExpressionBindings stdLib mempty storeExpr
          `shouldBe` Right storeExpr
      it "Replacing function with one that doesn't typecheck fails" $ do
        let storeExpr = getStoreExpression stdLib (getHashOfName stdLib (mkName "incrementInt"))
        let newHash = getHashOfName stdLib (mkName "id")
        let newBindings = Bindings (M.singleton (mkName "addInt") newHash)
        updateStoreExpressionBindings stdLib newBindings storeExpr
          `shouldSatisfy` isLeft
      it "Replacing function with an equivalent one succeeds" $ do
        let storeExpr = getStoreExpression stdLib (getHashOfName stdLib (mkName "incrementInt"))
        let newHash = getHashOfName stdLib (mkName "subtractInt")
        let newBindings = Bindings (M.singleton (mkName "addInt") newHash)
        updateStoreExpressionBindings stdLib newBindings storeExpr
          `shouldSatisfy` isRight
