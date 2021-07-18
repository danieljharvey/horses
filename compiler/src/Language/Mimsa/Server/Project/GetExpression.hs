{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.GetExpression
  ( getExpression,
    GetExpression,
  )
where

import qualified Data.Aeson as JSON
import Data.Swagger
import GHC.Generics
import qualified Language.Mimsa.Actions.Graph as Actions
import Language.Mimsa.Server.ExpressionData
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Servant

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
  (_, graphviz) <- fromActionM mimsaEnv projectHash (Actions.graphExpression se)
  (ResolvedExpression mt _ _ _ _) <-
    resolveStoreExpressionHandler project se
  writeStoreHandler mimsaEnv (prjStore project)
  GetExpressionResponse <$> expressionDataHandler project se mt graphviz
