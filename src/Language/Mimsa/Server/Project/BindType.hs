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
import Language.Mimsa.Typechecker.Codegen
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Servant

------

type BindType =
  "type"
    :> ReqBody '[JSON] BindTypeRequest
    :> Post '[JSON] BindTypeResponse

data BindTypeRequest = BindTypeRequest
  { btProjectHash :: ProjectHash,
    btExpression :: Text
  }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

data BindTypeResponse = BindTypeResponse
  { btProjectData :: ProjectData,
    btCodegenExprName :: Maybe Name,
    btTypeclasses :: [Typeclass]
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

bindType ::
  MimsaEnvironment ->
  BindTypeRequest ->
  Handler BindTypeResponse
bindType mimsaEnv (BindTypeRequest hash input) = do
  expr <- parseDataTypeHandler input
  (newProject, (typeClasses, codegenName)) <-
    fromActionM
      mimsaEnv
      hash
      (Actions.bindType input expr)
  pd <- projectDataHandler mimsaEnv newProject
  pure $
    BindTypeResponse pd codegenName typeClasses
