{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Server.Project.GetExpression
  ( getExpression,
    GetExpression,
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi
import GHC.Generics
import qualified Language.Mimsa.Actions.Helpers.Parse as Actions
import qualified Language.Mimsa.Actions.Graph as Actions
import qualified Language.Mimsa.Actions.Helpers.CanOptimise as Actions
import Language.Mimsa.Transform.Warnings
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Servant
import Server.Handlers
import Server.Helpers.ExpressionData
import Server.Types
import Language.Mimsa.Printer

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
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, ToSchema)

newtype GetExpressionResponse = GetExpressionResponse
  { geExpressionData :: ExpressionData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

getExpression ::
  MimsaEnvironment ->
  GetExpressionRequest ->
  Handler GetExpressionResponse
getExpression mimsaEnv (GetExpressionRequest projectHash exprHash') = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' projectHash
  se <- findExprHandler project exprHash'

  let action =
        (,,) <$> Actions.graphExpression se <*> Actions.canOptimise se
          <*> Actions.parseExpr (prettyPrint (storeExpression se))

  (_, (graphviz, canOptimise, exprName)) <-
    fromActionM
      mimsaEnv
      projectHash
      action

  -- re-resolved expr, using the parsed input expr (as it has annotations)
  resolvedExpr <-
    resolveStoreExpressionHandler project (se {storeExpression = exprName})

  writeStoreHandler mimsaEnv (prjStore project)

  -- | turn Expr Variable MonoType into Expr Name MonoType
  typedExpr <-
    useSwapsHandler
      (reSwaps resolvedExpr)
      (reTypedExpression resolvedExpr)
  let warnings = getWarnings resolvedExpr

  pure $
    GetExpressionResponse
      (makeExpressionData se typedExpr graphviz (reInput resolvedExpr) warnings canOptimise)
