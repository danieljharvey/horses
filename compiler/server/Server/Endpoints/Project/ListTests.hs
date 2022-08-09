{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Server.Endpoints.Project.ListTests
  ( listTestsHandler,
    ListTests,
    listTestsByExprHashHandler,
    ListTestsByExprHash,
  )
where

import qualified Data.Aeson as JSON
import qualified Data.Map.Strict as M
import Data.OpenApi
import GHC.Generics
import Language.Mimsa.Tests.Test
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Servant
import Server.Handlers
import Server.Helpers.TestData
import Server.Types

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
  project <- loadProjectHandler mimsaEnv hash
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

type ListTestsByExprHash =
  Capture "projectHash" ProjectHash
    :> "tests"
    :> "list"
    :> Capture "exprHash" ExprHash
    :> Get '[JSON] ListTestsByExprHashResponse

newtype ListTestsByExprHashResponse = ListTestsByExprHashResponse
  { ltbnTests :: TestData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

listTestsByExprHashHandler ::
  MimsaEnvironment ->
  ProjectHash ->
  ExprHash ->
  Handler ListTestsByExprHashResponse
listTestsByExprHashHandler mimsaEnv projectHash exprHash = do
  project <- loadProjectHandler mimsaEnv projectHash
  let tests = getTestsForExprHash project exprHash
  testResults <-
    runTestsHandler
      mimsaEnv
      project
      (M.elems tests)
  pure $
    ListTestsByExprHashResponse
      (makeTestData project testResults)
