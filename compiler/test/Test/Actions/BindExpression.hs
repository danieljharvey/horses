{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.BindExpression
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
import Data.Functor
import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.AddUnitTest as Actions
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.RemoveBinding as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Tests.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

brokenExpr :: Expr Name Annotation
brokenExpr = MyInfix mempty Equals (int 1) (bool True)

testWithIdInExpr :: Expr Name Annotation
testWithIdInExpr =
  unsafeParseExpr "id 1 == 1" $> mempty

spec :: Spec
spec = do
  describe "BindExpression" $ do
    it "Fails on a syntax error" $ do
      Actions.run
        testStdlib
        ( Actions.bindExpression
            brokenExpr
            "broken"
            "1 == True"
        )
        `shouldSatisfy` isLeft

    it "Adds a fresh new function to Bindings and to Store" $ do
      let expr = int 1
      case Actions.run testStdlib (Actions.bindExpression expr "one" "1") of
        Left _ -> error "Should not have failed"
        Right (newProject, outcomes, _) -> do
          -- one more item in store
          additionalStoreItems testStdlib newProject
            `shouldBe` 1
          -- one more binding
          lookupBindingName
            newProject
            "one"
            `shouldSatisfy` isJust
          -- one new store expression
          S.size (Actions.storeExpressionsFromOutcomes outcomes)
            `shouldBe` 1

    it "Updating an existing binding updates binding" $ do
      let newIdExpr = MyLambda mempty (Identifier mempty "b") (MyVar mempty "b")
      let action =
            Actions.bindExpression newIdExpr "id" "\\b -> b"
      case Actions.run testStdlib action of
        Left _ -> error "Should not have failed"
        Right (newProject, outcomes, _) -> do
          -- one more item, one updated test
          additionalStoreItems testStdlib newProject
            `shouldBe` 2
          -- two new expressions
          S.size (Actions.storeExpressionsFromOutcomes outcomes)
            `shouldBe` 2
          -- binding hash has changed
          lookupBindingName
            newProject
            "id"
            `shouldNotBe` lookupBindingName testStdlib "id"

    it "Updating an existing binding updates tests" $ do
      let newIdExpr =
            MyLambda
              mempty
              (Identifier mempty "blob")
              (MyVar mempty "blob")
      let action = do
            _ <-
              Actions.addUnitTest
                testWithIdInExpr
                (TestName "Check id is OK")
                (prettyPrint testWithIdInExpr)
            Actions.bindExpression newIdExpr "id" "\\blob -> blob"
      case Actions.run testStdlib action of
        Left _ -> error "Should not have failed"
        Right (newProject, outcomes, _) -> do
          -- four more items
          additionalStoreItems testStdlib newProject
            `shouldBe` 4
          -- one new expression, three new tests
          S.size (Actions.storeExpressionsFromOutcomes outcomes)
            `shouldBe` 4
          -- two more unit tests
          additionalTests testStdlib newProject
            `shouldBe` 3
          -- binding hash has changed
          lookupBindingName
            newProject
            "id"
            `shouldNotBe` lookupBindingName testStdlib "id"

    it "Re-binding an expression that uses a deleted binding does not break it" $ do
      let newIdExpr =
            MyLambda
              mempty
              (Identifier mempty "b")
              (MyVar mempty "b")
          useIdExpr =
            MyApp
              mempty
              (MyVar mempty "newId")
              (bool True)
          useIdExpr2 =
            MyApp
              mempty
              (MyVar mempty "newId")
              (bool False)
          action = do
            _ <- Actions.bindExpression newIdExpr "newId" (prettyPrint newIdExpr)
            _ <- Actions.bindExpression useIdExpr "useId" (prettyPrint useIdExpr)
            Actions.removeBinding "newId"
            Actions.bindExpression useIdExpr2 "useId" (prettyPrint useIdExpr2)
      Actions.run testStdlib action `shouldSatisfy` isRight
