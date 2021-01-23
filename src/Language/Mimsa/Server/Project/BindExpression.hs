{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.BindExpression
  ( bindExpression,
    BindExpression,
  )
where

import qualified Data.Aeson as JSON
import Data.Foldable (traverse_)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Project
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Servant

------

type BindExpression =
  "bind"
    :> ReqBody '[JSON] BindExpressionRequest
    :> Post '[JSON] BindExpressionResponse

data BindExpressionRequest = BindExpressionRequest
  { beProjectHash :: ProjectHash,
    beBindingName :: Name,
    beExpression :: Text
  }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

data BindExpressionResponse = BindExpressionResponse
  { beProjectData :: ProjectData,
    beExpressionData :: ExpressionData
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

bindExpression ::
  MimsaEnvironment ->
  BindExpressionRequest ->
  Handler BindExpressionResponse
bindExpression mimsaEnv (BindExpressionRequest hash name' input) = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' hash
  (ResolvedExpression mt se _ _ _) <-
    evaluateTextHandler project input
  exprHash <- saveExprHandler mimsaEnv se
  let newEnv = project <> fromItem name' se exprHash
  envWithTests <- updateUnitTestsHandler mimsaEnv newEnv exprHash name'
  writeStoreHandler mimsaEnv (prjStore envWithTests)
  pd <- projectDataHandler mimsaEnv envWithTests
  _ <- saveExprHandler mimsaEnv se
  ed <- expressionDataHandler project se mt
  pure $
    BindExpressionResponse
      pd
      ed

updateUnitTestsHandler ::
  MimsaEnvironment ->
  Project Annotation ->
  ExprHash ->
  Name ->
  Handler (Project Annotation)
updateUnitTestsHandler mimsaEnv project newExprHash name' = do
  case lookupBindingName project name' of
    Nothing -> pure project
    Just oldExprHash -> do
      (projectWithNewTests, newExprs) <-
        createNewUnitTestsHandler project oldExprHash newExprHash
      traverse_ (saveExprHandler mimsaEnv) newExprs
      pure projectWithNewTests
