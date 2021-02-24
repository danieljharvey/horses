{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.BindType
  ( bindType,
    BindType,
  )
where

import qualified Data.Aeson as JSON
import Data.Swagger
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.BindType as Actions
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Servant

------

type BindType =
  "bind"
    :> ReqBody '[JSON] BindTypeRequest
    :> Post '[JSON] BindTypeResponse

data BindTypeRequest = BindTypeRequest
  { beProjectHash :: ProjectHash,
    beBindingName :: Name,
    beExpression :: Text
  }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

data BindTypeResponse = BindTypeResponse
  { beProjectData :: ProjectData,
    beExpressionData :: ExpressionData,
    beUpdatedTestsCount :: Int
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

bindType ::
  MimsaEnvironment ->
  BindTypeRequest ->
  Handler BindTypeResponse
bindType mimsaEnv (BindTypeRequest hash name' input) = do
  expr <- parseDataTypeHandler input
  (newProject, (_typeclasses, _name)) <-
    fromActionM
      mimsaEnv
      hash
      (Actions.bindType input expr)
  pd <- projectDataHandler mimsaEnv newProject
  ed <- expressionDataHandler newProject se mt
  pure $
    BindTypeResponse
      pd
      ed
      numTests
