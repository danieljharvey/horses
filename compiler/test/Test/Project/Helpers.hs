{-# LANGUAGE OverloadedStrings #-}

module Test.Project.Helpers
  ( spec,
  )
where

import qualified Data.Map as M
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store.ExprHash
import Test.Hspec

spec :: Spec
spec =
  describe "Project Helpers" $ do
    describe "lookupOptimised" $ do
      it "Cannot find optimisation" $ do
        let prj = mempty
        lookupOptimised prj (ExprHash "123") `shouldBe` Nothing
      it "Finds self optimisation" $ do
        let prj = mempty {prjOptimised = M.fromList [(ExprHash "123", ExprHash "123")]}
        lookupOptimised prj (ExprHash "123") `shouldBe` Just (ExprHash "123")
      it "Finds other optimisation" $ do
        let prj = mempty {prjOptimised = M.fromList [(ExprHash "123", ExprHash "456")]}
        lookupOptimised prj (ExprHash "123") `shouldBe` Just (ExprHash "456")
      it "Finds other optimisation with self" $ do
        let prj = mempty {prjOptimised = M.fromList [(ExprHash "123", ExprHash "456"), (ExprHash "456", ExprHash "456")]}
        lookupOptimised prj (ExprHash "123") `shouldBe` Just (ExprHash "456")
      it "Finds chained optimisation" $ do
        let prj = mempty {prjOptimised = M.fromList [(ExprHash "123", ExprHash "456"), (ExprHash "456", ExprHash "789")]}
        lookupOptimised prj (ExprHash "123") `shouldBe` Just (ExprHash "789")
      it "Finds chained optimisation with self" $ do
        let prj = mempty {prjOptimised = M.fromList [(ExprHash "123", ExprHash "456"), (ExprHash "456", ExprHash "789"), (ExprHash "789", ExprHash "789")]}
        lookupOptimised prj (ExprHash "123") `shouldBe` Just (ExprHash "789")
