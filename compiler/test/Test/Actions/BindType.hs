{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Actions.BindType
  ( spec,
  )
where

import Data.Either (isRight)
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.BindType as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Test.Codegen.Shared
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

projectStoreSize :: Project ann -> Int
projectStoreSize = length . getStore . prjStore

spec :: Spec
spec = do
  describe "BindType" $ do
    it "Should bind Void" $ do
      let action = Actions.bindType (prettyPrint dtVoid) dtVoid
      let (newProject, outcomes, _) =
            fromRight (Actions.run testStdlib action)
      -- one more item in store
      projectStoreSize newProject
        `shouldBe` projectStoreSize testStdlib + 1
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
    it "Should bind Identity" $ do
      let action = Actions.bindType (prettyPrint dtIdentity) dtIdentity
      let (newProject, outcomes, _) = fromRight (Actions.run testStdlib action)
      -- one more items in store
      projectStoreSize newProject
        `shouldBe` projectStoreSize testStdlib + 1
      -- one more type binding
      lookupTypeBindingName
        newProject
        "Identity"
        `shouldSatisfy` isJust
      -- one new store expression
      S.size
        (Actions.storeExpressionsFromOutcomes outcomes)
        `shouldBe` 1
    it "Should bind TrafficLights" $ do
      let action = Actions.bindType (prettyPrint dtTrafficLights) dtTrafficLights
      let (newProject, outcomes, _) =
            fromRight (Actions.run testStdlib action)
      -- one more items in store
      projectStoreSize newProject
        `shouldBe` projectStoreSize testStdlib + 1
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
      -- one new store expressions
      S.size
        (Actions.storeExpressionsFromOutcomes outcomes)
        `shouldBe` 1

    it "Should bind ConsoleF without breaking" $ do
      let action = Actions.bindType (prettyPrint dtConsoleF) dtConsoleF
      Actions.run testStdlib action `shouldSatisfy` isRight

    it "Should bind Env without breaking" $ do
      let action = Actions.bindType (prettyPrint dtEnv) dtEnv
      Actions.run testStdlib action `shouldSatisfy` isRight
