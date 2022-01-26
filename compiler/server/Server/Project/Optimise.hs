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
import Data.Map (Map)
import qualified Data.Map as M
import Data.OpenApi hiding (get)
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.Helpers.Swaps as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Servant
import Server.Handlers
import Server.Helpers.ExpressionData
import Server.MimsaHandler
import Server.Types

-- /project/optimise/

type OptimiseAPI =
  "optimise" :> ReqBody '[JSON] OptimiseRequest
    :> JsonPost OptimiseResponse

data OptimiseRequest = OptimiseRequest
  { upBindingName :: Name,
    upProjectHash :: ProjectHash
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
  { upOptimisedDeps :: Map Text FromTo,
    upExpressionData :: ExpressionData,
    upProjectData :: ProjectData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

mapOptimisedDeps :: (Printer n) => Map ExprHash (n, ExprHash) -> Map Text FromTo
mapOptimisedDeps =
  M.fromList
    . fmap
      ( \(startHash, (ident, endHash)) ->
          (prettyPrint ident, FromTo startHash endHash)
      )
    . M.toList

optimise ::
  MimsaEnvironment ->
  OptimiseRequest ->
  MimsaHandler OptimiseResponse
optimise mimsaEnv (OptimiseRequest bindingName projectHash) =
  runMimsaHandlerT $ do
    let action = do
          se <-
            Actions.optimiseByName bindingName
          let (ResolvedExpression _ se _ _ swaps typedExpr input) = resolvedExpr
          typedNameExpr <- Actions.useSwaps swaps typedExpr
          pure
            ( mapOptimisedDeps depUpdates,
              makeExpressionData se typedNameExpr gv input
            )
    response <- lift $ eitherFromActionM mimsaEnv projectHash action
    case response of
      Left e -> throwMimsaError e
      Right (newProject, (optimisedDeps, ed)) -> do
        pd <- lift $ projectDataHandler mimsaEnv newProject
        returnMimsa $ OptimiseResponse optimisedDeps ed pd
