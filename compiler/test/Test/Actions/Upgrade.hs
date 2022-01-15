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
import qualified Language.Mimsa.Actions.AddUnitTest as Actions
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Upgrade as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Tests.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

newIdExpr :: Expr Name Annotation
newIdExpr = unsafeParseExpr "\\a -> let result = a in result" $> mempty

useBothExpr :: Expr Name Annotation
useBothExpr = unsafeParseExpr "const (id 100) True" $> mempty

newConstExpr :: Expr Name Annotation
newConstExpr = unsafeParseExpr "let dog = True in \\a -> \\b -> a" $> mempty

brokenExpr :: Expr Name Annotation
brokenExpr = unsafeParseExpr "dog" $> mempty

testForUseBoth :: Expr Name Annotation
testForUseBoth = unsafeParseExpr "useBoth == 100" $> mempty

spec :: Spec
spec = do
  describe "Upgrade" $ do
    it "Error when binding not found" $ do
      let action = Actions.upgradeByName "nonsenseNameThatDoesntExist"
      Actions.run testStdlib action `shouldSatisfy` isLeft
    it "Nothing to upgrade when expression has no dependencies" $ do
      let action = Actions.upgradeByName "id"
      let err = fromLeft $ Actions.run testStdlib action
      err `shouldBe` ProjectErr CantUpgradeNoDependencies
    it "Nothing to do when expression is up to date" $ do
      let action = do
            _ <- Actions.bindExpression useBothExpr "useBoth" (prettyPrint useBothExpr)
            Actions.upgradeByName "useBoth"
      let err = fromLeft $ Actions.run testStdlib action
      err `shouldBe` ProjectErr CantUpgradeAlreadyUpToDate
    it "Successfully updates to newest version" $ do
      let action = do
            _ <- Actions.bindExpression useBothExpr "useBoth" (prettyPrint useBothExpr)
            _ <- Actions.bindExpression newIdExpr "id" (prettyPrint newIdExpr)
            Actions.upgradeByName "useBoth"
      let (prj, actions, outcome) = fromRight $ Actions.run testStdlib action
      -- one dep was replaced in `useBoth`
      outcome
        `shouldSatisfy` \(_, replacements, _) -> M.size replacements == 1
      -- the two new items (`useBoth` and `id`) plus the upgraded one
      additionalStoreItems testStdlib prj `shouldBe` 3
      -- We logged a useful message
      Actions.messagesFromOutcomes actions
        `shouldSatisfy` elem "Updated useBoth. 1 dependency updated (id)"
    it "Repeat upgrade should return 'AlreadyUpToDate'" $ do
      let action = do
            _ <- Actions.bindExpression useBothExpr "useBoth" (prettyPrint useBothExpr)
            _ <- Actions.bindExpression newIdExpr "id" (prettyPrint newIdExpr)
            _ <- Actions.upgradeByName "useBoth"
            Actions.upgradeByName "useBoth"
      let err = fromLeft $ Actions.run testStdlib action
      err `shouldBe` ProjectErr CantUpgradeAlreadyUpToDate
    it "Fails if new dep does not typecheck" $ do
      let action = do
            _ <- Actions.bindExpression useBothExpr "useBoth" (prettyPrint useBothExpr)
            _ <- Actions.bindExpression brokenExpr "id" (prettyPrint brokenExpr)
            _ <- Actions.upgradeByName "useBoth"
            Actions.upgradeByName "useBoth"
      Actions.run testStdlib action `shouldSatisfy` isLeft

    it "Successfully updates two new deps to newest version" $ do
      let action = do
            _ <- Actions.bindExpression useBothExpr "useBoth" (prettyPrint useBothExpr)
            _ <- Actions.bindExpression newIdExpr "id" (prettyPrint newIdExpr)
            _ <- Actions.upgradeByName "useBoth"
            _ <- Actions.bindExpression newConstExpr "const" (prettyPrint newConstExpr)
            Actions.upgradeByName "useBoth"
      let (prj, actions, outcome) = fromRight $ Actions.run testStdlib action
      -- one deps were replaced in the last upgrade of `useBoth`
      outcome `shouldSatisfy` \(_, replacements, _) -> M.size replacements == 1
      -- the three new items (`useBoth`, `id`, `const`) plus the 2 upgraded
      -- ones
      additionalStoreItems testStdlib prj `shouldBe` 5
      -- We logged a useful message
      Actions.messagesFromOutcomes actions
        `shouldSatisfy` elem "Updated useBoth. 1 dependency updated (const)"

    it "Makes a copy of the test on update" $ do
      let action = do
            _ <- Actions.bindExpression useBothExpr "useBoth" (prettyPrint useBothExpr)
            _ <- Actions.bindExpression newIdExpr "id" (prettyPrint newIdExpr)
            _ <- Actions.addUnitTest testForUseBoth (TestName "Use useBoth") (prettyPrint testForUseBoth)
            Actions.upgradeByName "useBoth"
      let (prj, _, outcome) = fromRight $ Actions.run testStdlib action
      -- one deps were replaced in the last upgrade of `useBoth`
      outcome `shouldSatisfy` \(_, replacements, _) -> M.size replacements == 1
      -- the two new items (`useBoth`, `id`) plus the upgraded one plus two
      -- tests
      additionalStoreItems testStdlib prj `shouldBe` 5
      -- one test added manually, one created in the upgrade
      additionalTests testStdlib prj `shouldBe` 2
