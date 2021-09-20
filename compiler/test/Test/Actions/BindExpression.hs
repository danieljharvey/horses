{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.BindExpression
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.AddUnitTest as Actions
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.RemoveBinding as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

brokenExpr :: Expr Name Annotation
brokenExpr = MyInfix mempty Equals (int 1) (bool True)

projectStoreSize :: Project ann -> Int
projectStoreSize = length . getStore . prjStore

unitTestsSize :: Project ann -> Int
unitTestsSize = M.size . prjUnitTests

testWithIdInExpr :: Expr Name Annotation
testWithIdInExpr =
  MyInfix
    mempty
    Equals
    (MyApp mempty (MyVar mempty "id") (int 1))
    (int 1)

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
          projectStoreSize newProject
            `shouldBe` projectStoreSize testStdlib + 1
          -- one more binding
          lookupBindingName
            newProject
            "one"
            `shouldSatisfy` isJust
          -- one new store expression
          S.size (Actions.storeExpressionsFromOutcomes outcomes)
            `shouldBe` 1
    it "Updating an existing binding updates binding" $ do
      let newIdExpr = MyLambda mempty "b" (MyVar mempty "b")
      let action =
            Actions.bindExpression newIdExpr "id" "\\b -> b"
      case Actions.run testStdlib action of
        Left _ -> error "Should not have failed"
        Right (newProject, outcomes, _) -> do
          -- one more item
          projectStoreSize newProject
            `shouldBe` projectStoreSize testStdlib + 1
          -- one new expression
          S.size (Actions.storeExpressionsFromOutcomes outcomes)
            `shouldBe` 1
          -- binding hash has changed
          lookupBindingName
            newProject
            "id"
            `shouldNotBe` lookupBindingName testStdlib "id"
    it "Updating an existing binding updates tests" $ do
      let newIdExpr = MyLambda mempty "blob" (MyVar mempty "blob")
      let action = do
            _ <- Actions.addUnitTest testWithIdInExpr (TestName "Check id is OK") "id(1) == 1"
            Actions.bindExpression newIdExpr "id" "\\blob -> blob"
      case Actions.run testStdlib action of
        Left _ -> error "Should not have failed"
        Right (newProject, outcomes, _) -> do
          -- three more items
          projectStoreSize newProject
            `shouldBe` projectStoreSize testStdlib + 3
          -- one new expression, two new tests
          S.size (Actions.storeExpressionsFromOutcomes outcomes)
            `shouldBe` 3
          -- two more unit tests
          unitTestsSize newProject
            `shouldBe` unitTestsSize testStdlib + 2
          -- binding hash has changed
          lookupBindingName
            newProject
            "id"
            `shouldNotBe` lookupBindingName testStdlib "id"
    it "Re-binding an expression that uses a deleted binding does not break it" $ do
      let newIdExpr = MyLambda mempty "b" (MyVar mempty "b")
          useIdExpr = MyApp mempty (MyVar mempty "newId") (bool True)
          useIdExpr2 = MyApp mempty (MyVar mempty "newId") (bool False)
          action = do
            _ <- Actions.bindExpression newIdExpr "newId" (prettyPrint newIdExpr)
            _ <- Actions.bindExpression useIdExpr "useId" (prettyPrint useIdExpr)
            Actions.removeBinding "newId"
            Actions.bindExpression useIdExpr2 "useId" (prettyPrint useIdExpr2)
      Actions.run testStdlib action `shouldSatisfy` isRight
