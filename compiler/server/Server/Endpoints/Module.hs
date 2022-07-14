{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Server.Endpoints.Module
  ( moduleEndpoints,
    ModuleAPI,
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi hiding (Server)
import GHC.Generics
import qualified Language.Mimsa.Actions.Modules.Typecheck as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Types.Modules
import Servant
import Server.Handlers
import Server.Helpers.ModuleData
import Server.Types

type ModuleAPI = GetModule

moduleEndpoints :: MimsaEnvironment -> Server ModuleAPI
moduleEndpoints =
  getModule

-- /module/

type GetModule =
  "module"
    :> Capture "exprHash" ModuleHash
    :> Get '[JSON] GetModuleResponse

newtype GetModuleResponse = GetModuleResponse
  { geModuleData :: ModuleData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

getModule ::
  MimsaEnvironment ->
  ModuleHash ->
  Handler GetModuleResponse
getModule mimsaEnv modHash = do
  -- using items in the store, creating a project just for this module
  (storedModule, pd, _) <- projectFromModuleHandler mimsaEnv modHash

  let input = prettyPrint storedModule

  -- we want to reprint and parse the module in order to get annotations
  -- to show types on them
  let action =
        Actions.typecheckModule input storedModule

  (_, _, typedModule) <-
    fromActionM
      mimsaEnv
      (pdHash pd)
      action

  pure $
    GetModuleResponse
      (makeModuleData typedModule input)
