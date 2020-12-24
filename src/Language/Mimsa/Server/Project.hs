{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project
  ( projectEndpoints,
    ProjectAPI,
  )
where

import qualified Data.Aeson as JSON
import Data.Swagger
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Servant

-----

-- the Project endpoints output data in a way that front ends would be
-- interested

-- exprHashes should be strings to stop JS getting lost

type ProjectAPI =
  "project"
    :> ( EvaluateAPI :<|> ListBindings :<|> GetExpression
           :<|> CreateProject
           :<|> BindExpression
       )

projectEndpoints ::
  MimsaEnvironment ->
  Server ProjectAPI
projectEndpoints mimsaEnv =
  evaluateExpression mimsaEnv
    :<|> listBindings mimsaEnv
    :<|> getExpression mimsaEnv
    :<|> createProject mimsaEnv
    :<|> bindExpression mimsaEnv

-- /project/evaluate/

type EvaluateAPI =
  "evaluate" :> ReqBody '[JSON] EvaluateRequest
    :> Post '[JSON] EvaluateResponse

data EvaluateRequest = EvaluateRequest
  { erCode :: Text,
    erProjectHash :: ProjectHash
  }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

data EvaluateResponse = EvaluateResponse
  { erResult :: Text,
    erExpressionData :: ExpressionData
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

evaluateExpression ::
  MimsaEnvironment ->
  EvaluateRequest ->
  Handler EvaluateResponse
evaluateExpression mimsaEnv (EvaluateRequest code hash) = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' hash
  (ResolvedExpression mt se expr' scope' swaps) <-
    evaluateTextHandler project code
  simpleExpr <-
    interpretHandler scope' swaps expr'
  writeStoreHandler mimsaEnv (store project)
  EvaluateResponse (prettyPrint simpleExpr) <$> expressionDataHandler mimsaEnv se mt

-- /project/bindings/

type ListBindings =
  "bindings"
    :> ReqBody '[JSON] ListBindingsRequest
    :> Post '[JSON] ListBindingsResponse

newtype ListBindingsRequest = ListBindingsRequest
  {lbProjectHash :: ProjectHash}
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

newtype ListBindingsResponse = ListBindingsResponse
  { lbProjectData :: ProjectData
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

listBindings ::
  MimsaEnvironment ->
  ListBindingsRequest ->
  Handler ListBindingsResponse
listBindings mimsaEnv (ListBindingsRequest projectHash) = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' projectHash
  writeStoreHandler mimsaEnv (store project)
  ListBindingsResponse <$> projectDataHandler mimsaEnv project

-- /project/expression/

-- TODO: should this be in Store and have nothing to do with projects?
-- it could findExpr to get everything we need and then typecheck from there
type GetExpression =
  "expression" :> ReqBody '[JSON] GetExpressionRequest
    :> Post '[JSON] GetExpressionResponse

data GetExpressionRequest = GetExpressionRequest
  { geProjectHash :: ProjectHash,
    geExprHash :: ExprHash
  }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

newtype GetExpressionResponse = GetExpressionResponse
  { geExpressionData :: ExpressionData
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

getExpression ::
  MimsaEnvironment ->
  GetExpressionRequest ->
  Handler GetExpressionResponse
getExpression mimsaEnv (GetExpressionRequest projectHash exprHash') = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' projectHash
  se <- findExprHandler project exprHash'
  (ResolvedExpression mt _ _ _ _) <-
    resolveStoreExpressionHandler project se
  writeStoreHandler mimsaEnv (store project)
  GetExpressionResponse <$> expressionDataHandler mimsaEnv se mt

------

type CreateProject =
  "create"
    :> Get '[JSON] CreateProjectResponse

newtype CreateProjectResponse = CreateProjectResponse
  {cpProjectData :: ProjectData}
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

-- create an empty project
createProject ::
  MimsaEnvironment ->
  Handler CreateProjectResponse
createProject mimsaEnv =
  CreateProjectResponse <$> projectDataHandler mimsaEnv mempty

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
  writeStoreHandler mimsaEnv (store newEnv)
  pd <- projectDataHandler mimsaEnv newEnv
  ed <- expressionDataHandler mimsaEnv se mt
  pure $
    BindExpressionResponse
      pd
      ed
