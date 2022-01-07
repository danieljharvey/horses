{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.Upgrade
  ( spec,
  )
where

import Data.Either (isLeft)
import Data.Functor
import qualified Data.Map as M
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Upgrade as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

useIdExpr :: Expr Name Annotation
useIdExpr = unsafeParseExpr "id 100" $> mempty

newIdExpr :: Expr Name Annotation
newIdExpr = unsafeParseExpr "\\a -> let result = a in result" $> mempty

spec :: Spec
spec = do
  fdescribe "Upgrade" $ do
    it "Error when binding not found" $ do
      let action = Actions.upgradeByName "nonsenseNameThatDoesntExist"
      Actions.run testStdlib action `shouldSatisfy` isLeft
    it "Nothing to upgrade when expression has no dependencies" $ do
      let action = Actions.upgradeByName "id"
      let (prj, _, outcome) = fromRight $ Actions.run testStdlib action
      outcome `shouldBe` Actions.NoDependencies
      -- no new bindings
      prj `shouldBe` testStdlib
    it "Nothing to do when expression is up to date" $ do
      let action = do
            _ <- Actions.bindExpression useIdExpr "useId" (prettyPrint useIdExpr)
            Actions.upgradeByName "useId"
      let (prj, _, outcome) = fromRight $ Actions.run testStdlib action
      outcome `shouldBe` Actions.AlreadyUpToDate
      -- no new tests
      additionalTests testStdlib prj `shouldBe` 0
      -- one new store expression (`useId`)
      additionalStoreItems testStdlib prj `shouldBe` 1
    it "Successfully updates to newest version" $ do
      let action = do
            _ <- Actions.bindExpression useIdExpr "useId" (prettyPrint useIdExpr)
            _ <- Actions.bindExpression newIdExpr "id" (prettyPrint newIdExpr)
            Actions.upgradeByName "useId"
      let (prj, actions, outcome) = fromRight $ Actions.run testStdlib action
      -- one dep was replaced in `useId`
      outcome `shouldSatisfy` \case
        Actions.Updated _ replacements -> M.size replacements == 1
        _ -> False
      -- the two new items (`useId` and `id`) plus the upgraded one
      additionalStoreItems testStdlib prj `shouldBe` 3
      -- We logged a useful message
      Actions.messagesFromOutcomes actions
        `shouldSatisfy` elem "Updated useId. 1 dependency updated (id)"
