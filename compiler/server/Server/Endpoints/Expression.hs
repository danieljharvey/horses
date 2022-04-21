{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Server.Endpoints.Expression
  ( expressionEndpoints,
    ExpressionAPI,
  )
where

import qualified Data.Aeson as JSON
import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M
import Data.OpenApi hiding (Server)
import qualified Data.Text as T
import GHC.Generics
import qualified Language.Mimsa.Actions.Graph as Actions
import qualified Language.Mimsa.Actions.Helpers.CanOptimise as Actions
import qualified Language.Mimsa.Actions.Helpers.Parse as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Transform.Warnings
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Servant
import Server.Handlers
import Server.Helpers.ExpressionData
import Server.Types

instance FromHttpApiData [ExprHash] where
  parseUrlPiece t =
    traverse parseUrlPiece (T.splitOn "," t)
  parseQueryParam t =
    traverse parseQueryParam (T.splitOn "," t)

type ExpressionAPI = GetExpression :<|> GetExpressions

expressionEndpoints :: MimsaEnvironment -> Server ExpressionAPI
expressionEndpoints mimsaEnv =
  getExpression mimsaEnv
    :<|> getExpressions mimsaEnv

-- /expression/

type GetExpression =
  "expression" :> Capture "exprHash" ExprHash
    :> Get '[JSON] GetExpressionResponse

newtype GetExpressionResponse = GetExpressionResponse
  { geExpressionData :: ExpressionData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

getExpression ::
  MimsaEnvironment ->
  ExprHash ->
  Handler GetExpressionResponse
getExpression mimsaEnv exprHash' = do
  -- using items in the store, creating a project just for this expression
  (storeExpr, pd, _) <- projectFromExpressionHandler mimsaEnv exprHash'

  let action = do
        gv <- Actions.graphExpression storeExpr
        canOptimise <- Actions.canOptimise storeExpr
        let input = prettyPrint (storeExpression storeExpr)
        exprName <- Actions.parseExpr input
        resolvedExpr <- Actions.typecheckStoreExpression (storeExpr {storeExpression = exprName}) input
        pure (gv, canOptimise, resolvedExpr)

  (_, _, (graphviz, canOptimise, resolvedExpr)) <-
    fromActionM
      mimsaEnv
      (pdHash pd)
      action

  -- turn Expr Variable MonoType into Expr Name MonoType
  let typedExpr = first fst (reTypedExpression resolvedExpr)
  let warnings = getWarnings resolvedExpr

  pure $
    GetExpressionResponse
      (makeExpressionData storeExpr typedExpr graphviz (reInput resolvedExpr) warnings canOptimise)

----

type GetExpressions =
  "expressions" :> Capture "exprHashes" [ExprHash]
    :> Get '[JSON] GetExpressionsResponse

newtype GetExpressionsResponse = GetExpressionsResponse
  { geExpressionsData :: Map ExprHash ExpressionData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

getExpressions ::
  MimsaEnvironment ->
  [ExprHash] ->
  Handler GetExpressionsResponse
getExpressions mimsaEnv exprHashes = do
  -- using items in the store, creating a project just for this expression
  seAndProjects <- traverse (projectFromExpressionHandler mimsaEnv) exprHashes

  -- list of store expressions
  let storeExprs :: [StoreExpression Annotation]
      storeExprs = (\(a, _, _) -> a) <$> seAndProjects
      -- big project with everything in it
      combinedProject = mconcat ((\(_, _, c) -> c) <$> seAndProjects)

  pd <- projectDataHandler mimsaEnv combinedProject

  let action storeExpr = do
        gv <- Actions.graphExpression storeExpr
        canOptimise <- Actions.canOptimise storeExpr
        let input = prettyPrint (storeExpression storeExpr)
        exprName <- Actions.parseExpr input
        resolvedExpr <- Actions.typecheckStoreExpression (storeExpr {storeExpression = exprName}) input
        let typedExpr = first fst (reTypedExpression resolvedExpr)
        pure (gv, canOptimise, resolvedExpr, typedExpr)

  (_, _, results) <-
    fromActionM
      mimsaEnv
      (pdHash pd)
      (traverse action storeExprs)

  let create (graphviz, canOptimise, resolvedExpr, typedExpr) =
        let warnings = getWarnings resolvedExpr
            storeExpr = reStoreExpression resolvedExpr
            ed =
              makeExpressionData
                storeExpr
                typedExpr
                graphviz
                (reInput resolvedExpr)
                warnings
                canOptimise
         in M.singleton (getStoreExpressionHash storeExpr) ed

  pure $ GetExpressionsResponse $ mconcat (create <$> results)
