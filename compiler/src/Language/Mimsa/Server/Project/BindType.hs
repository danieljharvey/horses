{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.BindType
  ( bindType,
    BindType,
  )
where

import qualified Data.Aeson as JSON
import Data.Functor (($>))
import Data.Swagger
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.BindType as Actions
import Language.Mimsa.Codegen
import Language.Mimsa.Printer
import Language.Mimsa.Server.ExpressionData
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
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
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, ToSchema)

data CodegenInfo = CodegenInfo {ciExprName :: Name, ciHash :: ExprHash}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

data BindTypeResponse = BindTypeResponse
  { btProjectData :: ProjectData,
    btDataType :: DataType (),
    btPrettyType :: Text,
    btCodegen :: Maybe ExpressionData,
    btTypeclasses :: [Typeclass]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

bindType ::
  MimsaEnvironment ->
  BindTypeRequest ->
  Handler BindTypeResponse
bindType mimsaEnv (BindTypeRequest projectHash input) =
  do
    expr <- parseDataTypeHandler input
    (newProject, (typeClasses, codegenInfo, dt, gv)) <-
      fromActionM
        mimsaEnv
        projectHash
        (Actions.bindType input expr)
    pd <- projectDataHandler mimsaEnv newProject
    ed <- case codegenInfo of
      Just (ResolvedExpression mt se _ _ _) -> do
        ed' <- expressionDataHandler newProject se mt gv
        pure (Just ed')
      Nothing -> pure Nothing
    pure $
      BindTypeResponse pd (dt $> ()) (prettyPrint dt) ed typeClasses
