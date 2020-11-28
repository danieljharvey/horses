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
import Language.Mimsa.Types.AST
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
  Project Annotation ->
  Server ProjectAPI
projectEndpoints prj =
  evaluateExpression
    :<|> listBindings
    :<|> getExpression
    :<|> createProject prj
    :<|> bindExpression

-- /project/evaluate/

type EvaluateAPI =
  "evaluate" :> ReqBody '[JSON] EvaluateRequest
    :> Post '[JSON] EvaluateResponse

data EvaluateRequest
  = EvaluateRequest
      { erCode :: Text,
        erProjectHash :: ProjectHash
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

data EvaluateResponse
  = EvaluateResponse
      { erResult :: Text,
        erExpressionData :: ExpressionData
      }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

evaluateExpression ::
  EvaluateRequest ->
  Handler EvaluateResponse
evaluateExpression (EvaluateRequest code hash) = do
  project <- loadProjectHandler hash
  (ResolvedExpression mt se expr' scope' swaps) <-
    evaluateTextHandler project code
  simpleExpr <-
    interpretHandler scope' swaps expr'
  EvaluateResponse (prettyPrint simpleExpr) <$> expressionDataHandler se mt

-- /project/bindings/

type ListBindings =
  "bindings"
    :> ReqBody '[JSON] ListBindingsRequest
    :> Post '[JSON] ListBindingsResponse

newtype ListBindingsRequest
  = ListBindingsRequest
      {lbProjectHash :: ProjectHash}
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

newtype ListBindingsResponse
  = ListBindingsResponse
      { lbProjectData :: ProjectData
      }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

listBindings ::
  ListBindingsRequest ->
  Handler ListBindingsResponse
listBindings (ListBindingsRequest projectHash) = do
  project <- loadProjectHandler projectHash
  ListBindingsResponse <$> projectDataHandler project

-- /project/expression/

-- TODO: should this be in Store and have nothing to do with projects?
-- it could findExpr to get everything we need and then typecheck from there
type GetExpression =
  "expression" :> ReqBody '[JSON] GetExpressionRequest
    :> Post '[JSON] GetExpressionResponse

data GetExpressionRequest
  = GetExpressionRequest
      { geProjectHash :: ProjectHash,
        geExprHash :: ExprHash
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

newtype GetExpressionResponse
  = GetExpressionResponse
      { geExpressionData :: ExpressionData
      }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

getExpression ::
  GetExpressionRequest ->
  Handler GetExpressionResponse
getExpression (GetExpressionRequest projectHash exprHash') = do
  project <- loadProjectHandler projectHash
  se <- findExprHandler project exprHash'
  (ResolvedExpression mt _ _ _ _) <-
    resolveStoreExpressionHandler (store project) se
  GetExpressionResponse <$> expressionDataHandler se mt

------

type CreateProject =
  "create"
    :> Get '[JSON] CreateProjectResponse

newtype CreateProjectResponse
  = CreateProjectResponse
      {cpProjectData :: ProjectData}
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

-- creating a project in this case means copying the default one
createProject ::
  Project Annotation ->
  Handler CreateProjectResponse
createProject project =
  CreateProjectResponse <$> projectDataHandler project

------

type BindExpression =
  "bind"
    :> ReqBody '[JSON] BindExpressionRequest
    :> Post '[JSON] BindExpressionResponse

data BindExpressionRequest
  = BindExpressionRequest
      { beProjectHash :: ProjectHash,
        beBindingName :: Name,
        beExpression :: Text
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

data BindExpressionResponse
  = BindExpressionResponse
      { beProjectData :: ProjectData,
        beExpressionData :: ExpressionData
      }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

bindExpression ::
  BindExpressionRequest ->
  Handler BindExpressionResponse
bindExpression (BindExpressionRequest hash name' input) = do
  project <- loadProjectHandler hash
  (ResolvedExpression mt se _ _ _) <-
    evaluateTextHandler project input
  exprHash <- saveExprHandler se
  let newEnv = project <> fromItem name' se exprHash
  pd <- projectDataHandler newEnv
  ed <- expressionDataHandler se mt
  pure $
    BindExpressionResponse
      pd
      ed
