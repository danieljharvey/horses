{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Server.Project.Optimise
  ( optimise,
    OptimiseAPI,
  )
where

import Control.Monad.Except
import qualified Data.Aeson as JSON
import qualified Data.Map as M
import Data.OpenApi hiding (get)
import GHC.Generics
import qualified Language.Mimsa.Actions.Graph as Actions
import qualified Language.Mimsa.Actions.Helpers.Swaps as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
import Language.Mimsa.Transform.Warnings
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Servant
import Server.Handlers
import Server.Helpers.ExpressionData
import Server.Helpers.TestData
import Server.MimsaHandler
import Server.Types

-- /project/optimise/

type OptimiseAPI =
  "optimise" :> ReqBody '[JSON] OptimiseRequest
    :> JsonPost OptimiseResponse

data OptimiseRequest = OptimiseRequest
  { opBindingName :: Name,
    opProjectHash :: ProjectHash
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, ToSchema)

data FromTo = FromTo
  { from :: ExprHash,
    to :: ExprHash
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

data OptimiseResponse = OptimiseResponse
  { opExpressionData :: ExpressionData,
    opProjectData :: ProjectData,
    opTestData :: TestData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

optimise ::
  MimsaEnvironment ->
  OptimiseRequest ->
  MimsaHandler OptimiseResponse
optimise mimsaEnv (OptimiseRequest bindingName projectHash) =
  runMimsaHandlerT $ do
    let action = do
          (resolvedExpr, _) <-
            Actions.optimiseByName bindingName
          let (ResolvedExpression _ se _ _ swaps typedExpr input) = resolvedExpr
          typedNameExpr <- Actions.useSwaps swaps typedExpr
          graphviz <- Actions.graphExpression se
          let warnings = getWarnings resolvedExpr
          pure
            ( makeExpressionData
                se
                typedNameExpr
                graphviz
                input
                warnings
                False -- can't optimise this again, we just did
            )

    store' <- lift $ readStoreHandler mimsaEnv
    project <- lift $ loadProjectHandler mimsaEnv store' projectHash
    response <- lift $ eitherFromActionM mimsaEnv projectHash action

    tests <-
      lift $
        runTestsHandler
          mimsaEnv
          project
          (M.elems $ prjTests project)
    let testData = makeTestData project tests

    case response of
      Left e -> throwMimsaError e
      Right (newProject, ed) -> do
        pd <- lift $ projectDataHandler mimsaEnv newProject
        returnMimsa $ OptimiseResponse ed pd testData
