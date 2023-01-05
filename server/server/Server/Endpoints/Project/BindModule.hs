{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Server.Endpoints.Project.BindModule
  ( bindModule,
    BindModule,
  )
where

import Control.Monad.Trans.Class
import qualified Data.Aeson as JSON
import Data.OpenApi
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.Modules.Bind as Actions
import qualified Language.Mimsa.Actions.Modules.Check as Actions
import Language.Mimsa.Core
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Types.Project
import Servant
import Server.Handlers
import Server.Helpers.ModuleData
import Server.Helpers.ProjectData
import Server.Helpers.TestData
import Server.MimsaHandler
import Server.Types

------

type BindModule =
  "bind-module"
    :> ReqBody '[JSON] BindModuleRequest
    :> JsonPost BindModuleResponse

data BindModuleRequest = BindModuleRequest
  { bmProjectHash :: ProjectHash,
    bmModuleName :: Maybe ModuleName,
    bmModule :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, ToSchema)

data BindModuleResponse = BindModuleResponse
  { bmProjectData :: ProjectData,
    bmModuleData :: ModuleData,
    bmTestData :: TestData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

-- | if we get a module name, save it, if not, don't bother
bindModule ::
  MimsaEnvironment ->
  BindModuleRequest ->
  MimsaHandler BindModuleResponse
bindModule mimsaEnv (BindModuleRequest projectHash modName input) = runMimsaHandlerT $ do
  project <- lift $ loadProjectHandler mimsaEnv projectHash
  let action = do
        (newMod, testResults) <-
          Actions.checkModule (prjModuleStore project) input
        case modName of
          Just mName -> do
            _ <- Actions.bindModule (recoverAnn <$> newMod) mName input
            pure ()
          Nothing -> pure ()
        pure (makeModuleData newMod input, makeTestDataFromModule testResults)
  response <-
    lift $ eitherFromActionM mimsaEnv projectHash action
  case response of
    Right (newProject, _, (modData, testData)) -> do
      pd <- lift $ projectDataHandler mimsaEnv newProject
      returnMimsa $ BindModuleResponse pd modData testData
    Left e -> throwMimsaError e
