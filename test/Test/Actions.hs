{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
import Data.Functor
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.AddUnitTest as Actions
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.BindType as Actions
import qualified Language.Mimsa.Actions.Compile as Actions
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Typechecker.Codegen
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
import Test.Data.Project
import Test.Hspec
import Test.Typechecker.Codegen.Shared
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

onePlusOneExpr :: Expr Name Annotation
onePlusOneExpr = MyInfix mempty Add (int 1) (int 1)

fromRight :: (Printer e) => Either e a -> a
fromRight either' = case either' of
  Left e -> error (T.unpack $ prettyPrint e)
  Right a -> a

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
          Right (newProject, outcomes, _) -> do
            -- one more item in store
            projectStoreSize newProject
              `shouldBe` projectStoreSize stdLib + 1
            -- one more unit test
            unitTestsSize newProject
              `shouldBe` unitTestsSize stdLib + 1
            -- new expression
            S.size (Actions.storeExpressionsFromOutcomes outcomes) `shouldBe` 1
    describe "BindExpression" $ do
      it "Fails on a syntax error" $ do
        Actions.run
          stdLib
          ( Actions.bindExpression
              brokenExpr
              "broken"
              "1 == True"
          )
          `shouldSatisfy` isLeft
      it "Adds a fresh new function to Bindings and to Store" $ do
        let expr = int 1
        case Actions.run stdLib (Actions.bindExpression expr "one" "1") of
          Left _ -> error "Should not have failed"
          Right (newProject, outcomes, _) -> do
            -- one more item in store
            projectStoreSize newProject
              `shouldBe` projectStoreSize stdLib + 1
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
        case Actions.run stdLib action of
          Left _ -> error "Should not have failed"
          Right (newProject, outcomes, _) -> do
            -- one more item
            projectStoreSize newProject
              `shouldBe` projectStoreSize stdLib + 1
            -- one new expression
            S.size (Actions.storeExpressionsFromOutcomes outcomes)
              `shouldBe` 1
            -- binding hash has changed
            lookupBindingName
              newProject
              "id"
              `shouldNotBe` lookupBindingName stdLib "id"
      it "Updating an existing binding updates tests" $ do
        let newIdExpr = MyLambda mempty "blob" (MyVar mempty "blob")
        let action = do
              _ <- Actions.addUnitTest testWithIdInExpr (TestName "Check id is OK") "id(1) == 1"
              Actions.bindExpression newIdExpr "id" "\\blob -> blob"
        case Actions.run stdLib action of
          Left _ -> error "Should not have failed"
          Right (newProject, outcomes, _) -> do
            -- three more items
            projectStoreSize newProject
              `shouldBe` projectStoreSize stdLib + 3
            -- one new expression, two new tests
            S.size (Actions.storeExpressionsFromOutcomes outcomes)
              `shouldBe` 3
            -- two more unit tests
            unitTestsSize newProject
              `shouldBe` unitTestsSize stdLib + 2
            -- binding hash has changed
            lookupBindingName
              newProject
              "id"
              `shouldNotBe` lookupBindingName stdLib "id"
    describe "Compile" $ do
      it "Does not compile when expression does not match runtime" $ do
        let expr = MyLiteral mempty (MyInt 1)
        let action = Actions.compile consoleRuntime "1" expr
        let result = Actions.run stdLib action
        result `shouldSatisfy` isLeft
      it "Simplest compilation creates four files" $ do
        let expr = MyVar mempty "id"
        let action = Actions.compile exportRuntime "id" expr
        let (newProject, outcomes, (_, hashes)) = fromRight (Actions.run stdLib action)
        -- creates three files
        length (Actions.writeFilesFromOutcomes outcomes) `shouldBe` 4
        -- doesn't change project (for now)
        newProject `shouldBe` stdLib
        -- uses three different folders
        let uniqueFolders =
              nub
                ( (\(path, _, _) -> path)
                    <$> Actions.writeFilesFromOutcomes outcomes
                )
        length uniqueFolders `shouldBe` 3
        -- should have returned two exprHashs (one for the main expr, one
        -- for the `id` dependency
        S.size hashes `shouldBe` 2
      it "Complex compilation creates many files in 3 folders" $ do
        let expr = MyVar mempty "evalState"
        let action = Actions.compile exportRuntime "evalState" expr
        let (newProject, outcomes, _) = fromRight (Actions.run stdLib action)
        -- creates six files
        length (Actions.writeFilesFromOutcomes outcomes) `shouldBe` 7
        -- doesn't change project (for now)
        newProject `shouldBe` stdLib
        -- uses three different folders
        let uniqueFolders =
              nub
                ( (\(path, _, _) -> path)
                    <$> Actions.writeFilesFromOutcomes outcomes
                )
        length uniqueFolders `shouldBe` 3
    describe "Evaluate" $ do
      it "Should return an error for a broken expr" $ do
        Actions.run stdLib (Actions.evaluate (prettyPrint brokenExpr) brokenExpr) `shouldSatisfy` isLeft
      it "Should evaluate an expression" $ do
        let action = Actions.evaluate (prettyPrint onePlusOneExpr) onePlusOneExpr
        let (newProject, _, (mt, expr, _)) = fromRight (Actions.run stdLib action)
        mt $> () `shouldBe` MTPrim mempty MTInt
        expr $> () `shouldBe` int 2
        -- project should be untouched
        newProject `shouldBe` stdLib
    describe "BindType" $ do
      it "Should bind Void but create no functions" $ do
        let action = Actions.bindType (prettyPrint (dtVoid :: DataType ())) dtVoid
        let (newProject, outcomes, (outputs, _, _)) =
              fromRight (Actions.run stdLib action)
        -- no codegen matches this datatype
        outputs `shouldBe` mempty
        -- one more item in store
        projectStoreSize newProject
          `shouldBe` projectStoreSize stdLib + 1
        -- one more binding
        lookupBindingName
          newProject
          "void"
          `shouldSatisfy` isNothing
        -- one more type binding
        lookupTypeBindingName
          newProject
          "Void"
          `shouldSatisfy` isJust
        -- one new store expression
        S.size
          (Actions.storeExpressionsFromOutcomes outcomes)
          `shouldBe` 1
      it "Should bind Identity and create newtype and functor functions" $ do
        let action = Actions.bindType (prettyPrint (dtIdentity :: DataType ())) dtIdentity
        let (newProject, outcomes, (outputs, _, _)) = fromRight (Actions.run stdLib action)
        -- no codegen matches this datatype
        outputs `shouldBe` [Newtype, Functor, Foldable, Applicative]
        -- seven more items in store
        projectStoreSize newProject
          `shouldBe` projectStoreSize stdLib + 7
        -- one more binding
        lookupBindingName
          newProject
          "identity"
          `shouldSatisfy` isJust
        -- one more type binding
        lookupTypeBindingName
          newProject
          "Identity"
          `shouldSatisfy` isJust
        -- seven new store expression
        S.size
          (Actions.storeExpressionsFromOutcomes outcomes)
          `shouldBe` 7
      it "Should bind TrafficLights and create type bindings for constructors" $ do
        let action = Actions.bindType (prettyPrint (dtTrafficLights :: DataType ())) dtTrafficLights
        let (newProject, outcomes, (outputs, _, _)) =
              fromRight (Actions.run stdLib action)
        -- no codegen matches this datatype
        outputs `shouldBe` [Enum]
        -- three more items in store
        projectStoreSize newProject
          `shouldBe` projectStoreSize stdLib + 3
        -- one more binding
        lookupBindingName
          newProject
          "trafficLights"
          `shouldSatisfy` isJust
        -- four more type bindings
        lookupTypeBindingName
          newProject
          "Red"
          `shouldSatisfy` isJust
        -- four more type bindings
        lookupTypeBindingName
          newProject
          "Yellow"
          `shouldSatisfy` isJust
        -- four more type bindings
        lookupTypeBindingName
          newProject
          "Green"
          `shouldSatisfy` isJust
        -- three new store expressions
        S.size
          (Actions.storeExpressionsFromOutcomes outcomes)
          `shouldBe` 3

      it "Should bind ConsoleF without breaking" $ do
        let action = Actions.bindType (prettyPrint (dtConsoleF :: DataType ())) dtConsoleF
        Actions.run stdLib action `shouldSatisfy` isRight

      it "Should bind Env without breaking" $ do
        let action = Actions.bindType (prettyPrint (dtEnv :: DataType ())) dtEnv
        Actions.run stdLib action `shouldSatisfy` isRight
