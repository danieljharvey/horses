{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.ListBindings
  ( listBindings,
    ListBindings,
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi
import GHC.Generics
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.Project
import Servant

-- /project/bindings/

type ListBindings =
  "bindings"
    :> ReqBody '[JSON] ListBindingsRequest
    :> Post '[JSON] ListBindingsResponse

newtype ListBindingsRequest = ListBindingsRequest
  {lbProjectHash :: ProjectHash}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, ToSchema)

newtype ListBindingsResponse = ListBindingsResponse
  { lbProjectData :: ProjectData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

listBindings ::
  MimsaEnvironment ->
  ListBindingsRequest ->
  Handler ListBindingsResponse
listBindings mimsaEnv (ListBindingsRequest projectHash) = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' projectHash
  writeStoreHandler mimsaEnv (prjStore project)
  ListBindingsResponse <$> projectDataHandler mimsaEnv project
