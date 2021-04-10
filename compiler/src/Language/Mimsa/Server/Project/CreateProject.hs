{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.CreateProject
  ( createProject,
    CreateProject,
  )
where

import qualified Data.Aeson as JSON
import Data.Swagger
import GHC.Generics
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Types
import Servant

type CreateProject =
  "create"
    :> Get '[JSON] CreateProjectResponse

newtype CreateProjectResponse = CreateProjectResponse
  {cpProjectData :: ProjectData}
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

-- create an empty project
createProject ::
  MimsaEnvironment ->
  Handler CreateProjectResponse
createProject mimsaEnv =
  CreateProjectResponse <$> projectDataHandler mimsaEnv mempty
