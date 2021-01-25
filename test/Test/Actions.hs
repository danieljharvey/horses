{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions
  ( spec,
  )
where

import Data.Either (isLeft)
import Data.Functor
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.AddUnitTest as Actions
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
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
    (MyApp mempty (MyVar mempty (mkName "id")) (int 1))
    (int 1)

onePlusOneExpr :: Expr Name Annotation
onePlusOneExpr = MyInfix mempty Add (int 1) (int 1)

spec :: Spec
spec = do
  describe "Actions" $ do
    describe "AddUnitTest" $ do
      it "Fails with broken test" $ do
        Actions.run
          stdLib
          (Actions.addUnitTest brokenExpr (TestName "Oh no") "1 == True")
          `shouldSatisfy` isLeft
      it "Adds a new test" $ do
        case Actions.run
          stdLib
          (Actions.addUnitTest testWithIdInExpr (TestName "Id does nothing") "id(1) == 1") of
          Left _ -> error "Should not have failed"
          Right (newState, _) -> do
            -- one more item in store
            projectStoreSize (Actions.asProject newState)
              `shouldBe` projectStoreSize stdLib + 1
            -- one more unit test
            unitTestsSize (Actions.asProject newState)
              `shouldBe` unitTestsSize stdLib + 1
            -- new expression
            S.size (Actions.asStoreExpressions newState) `shouldBe` 1
    describe "BindExpression" $ do
      it "Fails on a syntax error" $ do
        Actions.run
          stdLib
          ( Actions.bindExpression
              brokenExpr
              (mkName "broken")
              "1 == True"
          )
          `shouldSatisfy` isLeft
      it "Adds a fresh new function to Bindings and to Store" $ do
        let expr = int 1
        case Actions.run stdLib (Actions.bindExpression expr (mkName "one") "1") of
          Left _ -> error "Should not have failed"
          Right (newState, _) -> do
            -- one more item in store
            projectStoreSize (Actions.asProject newState)
              `shouldBe` projectStoreSize stdLib + 1
            -- one more binding
            lookupBindingName
              (Actions.asProject newState)
              (mkName "one")
              `shouldSatisfy` isJust
            -- one new store expression
            S.size (Actions.asStoreExpressions newState)
              `shouldBe` 1
      it "Updating an existing binding updates binding" $ do
        let newIdExpr = MyLambda mempty (mkName "b") (MyVar mempty (mkName "b"))
        let action =
              Actions.bindExpression newIdExpr (mkName "id") "\\b -> b"
        case Actions.run stdLib action of
          Left _ -> error "Should not have failed"
          Right (newState, _) -> do
            -- one more item
            projectStoreSize (Actions.asProject newState)
              `shouldBe` projectStoreSize stdLib + 1
            -- one new expression
            S.size (Actions.asStoreExpressions newState)
              `shouldBe` 1
            -- binding hash has changed
            lookupBindingName
              (Actions.asProject newState)
              (mkName "id")
              `shouldNotBe` lookupBindingName stdLib (mkName "id")
      it "Updating an existing binding updates tests" $ do
        let newIdExpr = MyLambda mempty (mkName "blob") (MyVar mempty (mkName "blob"))
        let action = do
              _ <- Actions.addUnitTest testWithIdInExpr (TestName "Check id is OK") "id(1) == 1"
              Actions.bindExpression newIdExpr (mkName "id") "\\blob -> blob"
        case Actions.run stdLib action of
          Left _ -> error "Should not have failed"
          Right (newState, _) -> do
            -- three more items
            projectStoreSize (Actions.asProject newState)
              `shouldBe` projectStoreSize stdLib + 3
            -- one new expression, two new tests
            S.size (Actions.asStoreExpressions newState)
              `shouldBe` 3
            -- two more unit tests
            unitTestsSize (Actions.asProject newState)
              `shouldBe` unitTestsSize stdLib + 2
            -- binding hash has changed
            lookupBindingName
              (Actions.asProject newState)
              (mkName "id")
              `shouldNotBe` lookupBindingName stdLib (mkName "id")
    describe "Evaluate" $ do
      it "Should return an error for a broken expr" $ do
        Actions.run stdLib (Actions.evaluate (prettyPrint brokenExpr) brokenExpr) `shouldSatisfy` isLeft
      it "Should evaluate an expression" $ do
        case Actions.run stdLib (Actions.evaluate (prettyPrint onePlusOneExpr) onePlusOneExpr) of
          Left _ -> error "Should not have failed"
          Right (Actions.ActionState newProject _ _, (mt, expr, _)) -> do
            mt $> () `shouldBe` MTPrim mempty MTInt
            expr $> () `shouldBe` int 2
            -- project should be untouched
            newProject `shouldBe` stdLib
