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
import Data.Map (Map)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Actions (evaluateText, resolveStoreExpression)
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Server.Helpers
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Servant

-----

outputBindings :: Project a -> Map Name Text
outputBindings project =
  prettyPrint
    <$> getBindings
      ( getCurrentBindings
          (bindings project)
      )

outputTypeBindings :: Project a -> Map TyCon Text
outputTypeBindings project =
  prettyPrint
    <$> getTypeBindings
      (getCurrentTypeBindings (typeBindings project))

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
        erProject :: Project Annotation
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

data EvaluateResponse
  = EvaluateResponse
      { prettyExpr :: Text,
        prettyType :: Text,
        prettyHash :: Text,
        prettyResult :: Text
      }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

evaluateExpression ::
  EvaluateRequest ->
  Handler EvaluateResponse
evaluateExpression body = do
  (ResolvedExpression mt se expr' scope' swaps) <-
    handleEither UserError (evaluateText (erProject body) (erCode body))
  simpleExpr <-
    handleEither InternalError (interpret scope' swaps expr')
  exprHash' <-
    handleExceptT InternalError (saveExpr se)
  let prettyExpr' = prettyPrint (storeExpression se)
      prettyType' = prettyPrint mt
      prettyHash' = prettyPrint exprHash'
      prettyResult' = prettyPrint simpleExpr
  pure (EvaluateResponse prettyExpr' prettyType' prettyHash' prettyResult')

-- /project/bindings/

type ListBindings =
  "bindings"
    :> ReqBody '[JSON] ListBindingsRequest
    :> Get '[JSON] ListBindingsResponse

newtype ListBindingsRequest
  = ListBindingsRequest
      {lbProject :: Project Annotation}
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

data ListBindingsResponse
  = ListBindingsResponse
      { projectBindings :: Map Name Text,
        projectTypeBindings :: Map TyCon Text
      }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

listBindings ::
  ListBindingsRequest ->
  Handler ListBindingsResponse
listBindings (ListBindingsRequest project) =
  pure $
    ListBindingsResponse
      (outputBindings project)
      (outputTypeBindings project)

-- /project/expression/

-- TODO: should this be in Store and have nothing to do with projects?
-- it could findExpr to get everything we need and then typecheck from there
type GetExpression =
  "expression" :> ReqBody '[JSON] GetExpressionRequest
    :> Get '[JSON] GetExpressionResponse

data GetExpressionRequest
  = GetExpressionRequest
      { geProject :: Project Annotation,
        geExprHash :: ExprHash
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

data GetExpressionResponse
  = GetExpressionResponse
      { exprValue :: Text,
        exprType :: Text,
        exprBindings :: Map Name Text,
        exprTypeBindings :: Map TyCon Text
      }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

getExpression ::
  GetExpressionRequest ->
  Handler GetExpressionResponse
getExpression (GetExpressionRequest project exprHash') = do
  se <- handleEither InternalError $
    case lookupExprHash project exprHash' of
      Nothing -> Left ("Could not find exprhash!" :: Text)
      Just a -> Right a
  (ResolvedExpression mt _ _ _ _) <- handleEither UserError $ resolveStoreExpression (store project) "" se
  pure
    ( GetExpressionResponse
        (prettyPrint (storeExpression se))
        (prettyPrint mt)
        (prettyPrint <$> getBindings (storeBindings se))
        (prettyPrint <$> getTypeBindings (storeTypeBindings se))
    )

------

type CreateProject =
  "create"
    :> Get '[JSON] CreateProjectResponse

data CreateProjectResponse
  = CreateProjectResponse
      { cpData :: Project Annotation,
        cpBindings :: Map Name Text,
        cpTypeBindings :: Map TyCon Text
      }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

-- creating a project in this case means copying the default one
createProject ::
  Project Annotation ->
  Handler CreateProjectResponse
createProject project =
  pure $
    CreateProjectResponse
      project
      (outputBindings project)
      (outputTypeBindings project)

------

type BindExpression =
  "bind"
    :> ReqBody '[JSON] BindExpressionRequest
    :> Post '[JSON] BindExpressionResponse

data BindExpressionRequest
  = BindExpressionRequest
      { beProject :: Project Annotation,
        beBindingName :: Name,
        beExpression :: Text
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

data BindExpressionResponse
  = BindExpressionResponse
      { beData :: Project Annotation,
        beBindings :: Map Name Text,
        beTypeBindings :: Map TyCon Text,
        bePrettyExpr :: Text,
        bePrettyType :: Text,
        bePrettyHash :: Text
      }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

bindExpression ::
  BindExpressionRequest ->
  Handler BindExpressionResponse
bindExpression (BindExpressionRequest project name' input) = do
  (ResolvedExpression mt se _ _ _) <-
    handleEither UserError (evaluateText project input)
  hash <- handleExceptT InternalError (saveExpr se)
  let newEnv = project <> fromItem name' se hash
  pure $
    BindExpressionResponse
      newEnv
      (outputBindings newEnv)
      (outputTypeBindings newEnv)
      (prettyPrint (storeExpression se))
      (prettyPrint mt)
      (prettyPrint hash)
