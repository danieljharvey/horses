{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.BindType
  ( spec,
  )
where

import Data.Either (isRight)
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.BindType as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Typechecker.Codegen
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Typechecker.Codegen.Shared

projectStoreSize :: Project ann -> Int
projectStoreSize = length . getStore . prjStore

fromRight :: (Printer e) => Either e a -> a
fromRight either' = case either' of
  Left e -> error (T.unpack $ prettyPrint e)
  Right a -> a

spec :: Spec
spec = do
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
