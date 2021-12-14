{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.GetExpression
  ( getExpression,
    GetExpression,
  )
where

import qualified Data.Aeson as JSON
import qualified Data.Map as M
import Data.OpenApi
import GHC.Generics
import qualified Language.Mimsa.Actions.Graph as Actions
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Helpers.ExpressionData
import Language.Mimsa.Server.Helpers.TestData
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
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, ToSchema)

data GetExpressionResponse = GetExpressionResponse
  { geExpressionData :: ExpressionData,
    geTestData :: TestData
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
  (_, graphviz) <-
    fromActionM
      mimsaEnv
      projectHash
      (Actions.graphExpression se)
  resolvedExpr <-
    resolveStoreExpressionHandler project se
  writeStoreHandler mimsaEnv (prjStore project)
  typedExpr <-
    useSwapsHandler
      (reSwaps resolvedExpr)
      (reTypedExpression resolvedExpr)
  tests <-
    runTestsHandler
      mimsaEnv
      project
      (M.elems $ prjTests project)
  pure $
    GetExpressionResponse
      (makeExpressionData se typedExpr graphviz (reInput resolvedExpr))
      (makeTestData project tests)
