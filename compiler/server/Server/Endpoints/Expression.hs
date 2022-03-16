{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Server.Endpoints.Expression
  ( getExpression,
    GetExpression,
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi
import GHC.Generics
import qualified Language.Mimsa.Actions.Graph as Actions
import qualified Language.Mimsa.Actions.Helpers.CanOptimise as Actions
import qualified Language.Mimsa.Actions.Helpers.Parse as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Transform.Warnings
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Servant
import Server.Handlers
import Server.Helpers.ExpressionData
import Server.Types

-- /expression/

type GetExpression =
  "expression" :> Capture "exprHash" ExprHash
    :> Get '[JSON] GetExpressionResponse

newtype GetExpressionRequest = GetExpressionRequest
  { geExprHash :: ExprHash
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, ToSchema)

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

  (_, (graphviz, canOptimise, resolvedExpr)) <-
    fromActionM
      mimsaEnv
      (pdHash pd)
      action

  -- turn Expr Variable MonoType into Expr Name MonoType
  typedExpr <-
    useSwapsHandler
      (reSwaps resolvedExpr)
      (reTypedExpression resolvedExpr)
  let warnings = getWarnings resolvedExpr

  pure $
    GetExpressionResponse
      (makeExpressionData storeExpr typedExpr graphviz (reInput resolvedExpr) warnings canOptimise)
