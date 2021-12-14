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
import Language.Mimsa.Server.Types
import Language.Mimsa.Tests.Test
import Language.Mimsa.Tests.Types
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Servant

------

type ListTests =
  Capture "projectHash" ProjectHash
    :> "tests"
    :> "list"
    :> Get '[JSON] ListTestsResponse

data ListTestsResponse = ListTestsResponse
  { ltUnitTests :: [UnitTest],
    ltPropertyTests :: [PropertyTest]
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
  pure $
    ListTestsResponse
      (M.elems $ M.mapMaybe filterUnitTest $ prjTests project)
      (M.elems $ M.mapMaybe filterPropertyTest $ prjTests project)

----

type ListTestsByName =
  Capture "projectHash" ProjectHash
    :> "tests"
    :> "list"
    :> Capture "name" Name
    :> Get '[JSON] ListTestsByNameResponse

data ListTestsByNameResponse = ListTestsByNameResponse
  { ltbnUnitTests :: [UnitTest],
    ltbnPropertyTests :: [PropertyTest]
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
  pure $
    ListTestsByNameResponse
      (M.elems $ M.mapMaybe filterUnitTest tests)
      (M.elems $ M.mapMaybe filterPropertyTest tests)
