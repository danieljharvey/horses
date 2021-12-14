{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.ListTests
  ( listTestsHandler,
    ListTests,
    listTestsByNameHandler,
    ListTestsByName,
  )
where

import qualified Data.Aeson as JSON
import qualified Data.Map as M
import Data.OpenApi
import GHC.Generics
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Helpers.TestData
import Language.Mimsa.Server.Types
import Language.Mimsa.Tests.Test
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Servant

------

type ListTests =
  Capture "projectHash" ProjectHash
    :> "tests"
    :> "list"
    :> Get '[JSON] ListTestsResponse

newtype ListTestsResponse = ListTestsResponse
  { ltTests :: TestData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

listTestsHandler ::
  MimsaEnvironment ->
  ProjectHash ->
  Handler ListTestsResponse
listTestsHandler mimsaEnv hash = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' hash
  let tests = M.elems (prjTests project)
  testResults <-
    runTestsHandler
      mimsaEnv
      project
      tests
  pure $
    ListTestsResponse
      (makeTestData project testResults)

----

type ListTestsByName =
  Capture "projectHash" ProjectHash
    :> "tests"
    :> "list"
    :> Capture "name" Name
    :> Get '[JSON] ListTestsByNameResponse

newtype ListTestsByNameResponse = ListTestsByNameResponse
  { ltbnTests :: TestData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

listTestsByNameHandler ::
  MimsaEnvironment ->
  ProjectHash ->
  Name ->
  Handler ListTestsByNameResponse
listTestsByNameHandler mimsaEnv hash name' = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' hash
  let tests = case lookupBindingName project name' of
        Just exprHash -> getTestsForExprHash project exprHash
        Nothing -> mempty
  testResults <-
    runTestsHandler
      mimsaEnv
      project
      (M.elems tests)
  pure $
    ListTestsByNameResponse
      (makeTestData project testResults)
