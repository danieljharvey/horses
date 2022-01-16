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
import Language.Mimsa.Codegen
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
    it "Should bind Void but create no functions" $ do
      let action = Actions.bindType (prettyPrint dtVoid) dtVoid
      let (newProject, outcomes, (outputs, _, _, _)) =
            fromRight (Actions.run testStdlib action)
      -- no codegen matches this datatype
      outputs `shouldBe` mempty
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
    it "Should bind Identity and create newtype and functor functions" $ do
      let action = Actions.bindType (prettyPrint dtIdentity) dtIdentity
      let (newProject, outcomes, (outputs, _, _, _)) = fromRight (Actions.run testStdlib action)
      -- no codegen matches this datatype
      outputs `shouldBe` [Newtype, Functor, Foldable, Applicative]
      -- seven more items in store
      projectStoreSize newProject
        `shouldBe` projectStoreSize testStdlib + 7
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
      let action = Actions.bindType (prettyPrint dtTrafficLights) dtTrafficLights
      let (newProject, outcomes, (outputs, _, _, _)) =
            fromRight (Actions.run testStdlib action)
      -- no codegen matches this datatype
      outputs `shouldBe` [Enum]
      -- four more items in store
      projectStoreSize newProject
        `shouldBe` projectStoreSize testStdlib + 4
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
      -- four new store expressions
      S.size
        (Actions.storeExpressionsFromOutcomes outcomes)
        `shouldBe` 4

    it "Should bind ConsoleF without breaking" $ do
      let action = Actions.bindType (prettyPrint dtConsoleF) dtConsoleF
      Actions.run testStdlib action `shouldSatisfy` isRight

    it "Should bind Env without breaking" $ do
      let action = Actions.bindType (prettyPrint dtEnv) dtEnv
      Actions.run testStdlib action `shouldSatisfy` isRight
