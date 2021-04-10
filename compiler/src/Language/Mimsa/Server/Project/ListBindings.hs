{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.ListBindings
  ( listBindings,
    ListBindings,
  )
where

import qualified Data.Aeson as JSON
import Data.Swagger
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
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

newtype ListBindingsResponse = ListBindingsResponse
  { lbProjectData :: ProjectData
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

listBindings ::
  MimsaEnvironment ->
  ListBindingsRequest ->
  Handler ListBindingsResponse
listBindings mimsaEnv (ListBindingsRequest projectHash) = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' projectHash
  writeStoreHandler mimsaEnv (prjStore project)
  ListBindingsResponse <$> projectDataHandler mimsaEnv project
