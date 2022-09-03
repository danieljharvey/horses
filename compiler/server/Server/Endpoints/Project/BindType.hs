{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Server.Endpoints.Project.BindType
  ( bindType,
    BindType,
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.BindType as Actions
import qualified Language.Mimsa.Actions.Helpers.Parse as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Servant
import Server.Handlers
import Server.MimsaHandler
import Server.Types

------

type BindType =
  "type"
    :> ReqBody '[JSON] BindTypeRequest
    :> JsonPost BindTypeResponse

data BindTypeRequest = BindTypeRequest
  { btProjectHash :: ProjectHash,
    btExpression :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, ToSchema)

data CodegenInfo = CodegenInfo {ciExprName :: Name, ciHash :: ExprHash}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

data BindTypeResponse = BindTypeResponse
  { btProjectData :: ProjectData,
    btPrettyType :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

bindType ::
  MimsaEnvironment ->
  BindTypeRequest ->
  MimsaHandler BindTypeResponse
bindType mimsaEnv (BindTypeRequest projectHash input) = runMimsaHandlerT $ do
  let action = do
        expr <- Actions.parseDataType input
        Actions.bindType input expr
  response <-
    lift $ eitherFromActionM mimsaEnv projectHash action
  case response of
    Right (newProject, _, dt) -> do
      pd <- lift $ projectDataHandler mimsaEnv newProject
      returnMimsa $
        BindTypeResponse pd (prettyPrint dt)
    Left e -> throwMimsaError e
