{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Swagger
import GHC.Generics
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.UnitTest
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Servant

------

type ListTests =
  "tests" :> "list"
    :> ReqBody '[JSON] ListTestsRequest
    :> Get '[JSON] ListTestsResponse

newtype ListTestsRequest = ListTestsRequest
  { ltProjectHash :: ProjectHash
  }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

newtype ListTestsResponse = ListTestsResponse
  { ltUnitTests :: [UnitTest]
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

listTestsHandler ::
  MimsaEnvironment ->
  ListTestsRequest ->
  Handler ListTestsResponse
listTestsHandler mimsaEnv (ListTestsRequest hash) = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' hash
  pure $ ListTestsResponse (M.elems $ prjUnitTests project)

----

type ListTestsByName =
  "tests"
    :> "list"
    :> Capture "name" Name
    :> ReqBody '[JSON] ListTestsByNameRequest
    :> Get '[JSON] ListTestsByNameResponse

newtype ListTestsByNameRequest = ListTestsByNameRequest
  { ltbnProjectHash :: ProjectHash
  }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

newtype ListTestsByNameResponse = ListTestsByNameResponse
  { ltbnUnitTests :: [UnitTest]
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

listTestsByNameHandler ::
  MimsaEnvironment ->
  Name ->
  ListTestsByNameRequest ->
  Handler ListTestsByNameResponse
listTestsByNameHandler mimsaEnv name' (ListTestsByNameRequest hash) = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' hash
  let tests = case lookupBindingName project name' of
        Just exprHash -> getTestsForExprHash project exprHash
        Nothing -> mempty
  pure (ListTestsByNameResponse (M.elems tests))
