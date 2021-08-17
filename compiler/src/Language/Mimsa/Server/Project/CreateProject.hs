{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.CreateProject
  ( createProject,
    CreateProject,
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi
import GHC.Generics
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Types
import Servant

type CreateProject =
  "create"
    :> Get '[JSON] CreateProjectResponse

newtype CreateProjectResponse = CreateProjectResponse
  {cpProjectData :: ProjectData}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

-- create an empty project
createProject ::
  MimsaEnvironment ->
  Handler CreateProjectResponse
createProject mimsaEnv =
  CreateProjectResponse <$> projectDataHandler mimsaEnv stdlib
