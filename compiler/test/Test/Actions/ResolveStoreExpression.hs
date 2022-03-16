{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.ResolveStoreExpression
  ( spec,
  )
where

import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.ResolveStoreExpression as Actions
import Language.Mimsa.Printer
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "ResolveStoreExpression" $ do
    it "Resolves an item from the test stdlib" $ do
      let inputHash = getHashOfName testStdlib "liftA2State"
      -- get an arbitrary store expression from test project
      let storeExpr = getStoreExpression testStdlib inputHash
      -- resolve it
      let action = do
            Actions.resolveStoreExpression storeExpr (prettyPrint storeExpr)
      -- run action
      let (newProject, _, _resolved) =
            fromRight (Actions.run testStdlib action)
      -- it should not have changed the project
      newProject `shouldBe` testStdlib
