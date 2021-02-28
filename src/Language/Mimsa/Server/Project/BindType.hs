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
import Language.Mimsa.Printer
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Types
import Language.Mimsa.Typechecker.Codegen
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
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

data CodegenInfo = CodegenInfo {ciExprName :: Name, ciHash :: ExprHash}
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

data BindTypeResponse = BindTypeResponse
  { btProjectData :: ProjectData,
    btDataType :: DataType,
    btPrettyType :: Text,
    btCodegen :: Maybe CodegenInfo,
    btTypeclasses :: [Typeclass]
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

bindType ::
  MimsaEnvironment ->
  BindTypeRequest ->
  Handler BindTypeResponse
bindType mimsaEnv (BindTypeRequest projectHash input) =
  do
    expr <- parseDataTypeHandler input
    (newProject, (typeClasses, codegenInfo, dt)) <-
      fromActionM
        mimsaEnv
        projectHash
        (Actions.bindType input expr)
    pd <- projectDataHandler mimsaEnv newProject
    let codegen =
          (\(name', hash) -> CodegenInfo name' hash)
            <$> codegenInfo
    pure $
      BindTypeResponse pd dt (prettyPrint dt) codegen typeClasses
