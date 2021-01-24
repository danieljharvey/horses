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
    beExpressionData :: ExpressionData,
    beUpdatedTestsCount :: Int
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

-- TODO - load unit test expressions
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
  (numTests, projectWithTests) <-
    updateUnitTestsHandler
      mimsaEnv
      (project <> fromStoreExpression se exprHash)
      exprHash
      name'
  let newProject = projectWithTests <> fromItem name' se exprHash
  writeStoreHandler mimsaEnv (prjStore newProject)
  pd <- projectDataHandler mimsaEnv newProject
  _ <- saveExprHandler mimsaEnv se
  ed <- expressionDataHandler newProject se mt
  pure $
    BindExpressionResponse
      pd
      ed
      numTests

updateUnitTestsHandler ::
  MimsaEnvironment ->
  Project Annotation ->
  ExprHash ->
  Name ->
  Handler (Int, Project Annotation)
updateUnitTestsHandler mimsaEnv project newExprHash name' = do
  case lookupBindingName project name' of
    Nothing -> pure (0, project)
    Just oldExprHash -> do
      (projectWithNewTests, newExprs) <-
        createNewUnitTestsHandler project oldExprHash newExprHash
      traverse_ (saveExprHandler mimsaEnv) newExprs
      pure (length newExprs, projectWithNewTests)
