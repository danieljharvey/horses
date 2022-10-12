{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Helpers.ProjectData
  ( ProjectData (..),
    projectDataHandler,
  )
where

import qualified Data.Aeson as JSON
import Data.Map.Strict (Map)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Project
import Servant
import Server.Helpers
import Server.Persistence
import Server.Types

outputModuleBindings :: Project a -> Map ModuleName Text
outputModuleBindings project =
  prettyPrint
    <$> getCurrentModules (prjModules project)

-- | Current state of the project
-- should contain no exprs
data ProjectData = ProjectData
  { pdHash :: ProjectHash,
    pdModuleBindings :: Map ModuleName Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

-- given a new Project, save it and return the hash and bindings
projectDataHandler ::
  MimsaEnvironment ->
  Project ann ->
  Handler ProjectData
projectDataHandler mimsaEnv project = do
  -- save project file
  projHash <-
    handleServerM
      (mimsaConfig mimsaEnv)
      InternalError
      (saveProjectInStore project)

  pure $
    ProjectData
      projHash
      (outputModuleBindings project)
