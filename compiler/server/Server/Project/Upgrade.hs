{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Server.Project.Upgrade
  ( upgrade,
    UpgradeAPI,
  )
where

import Control.Monad.Except
import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import Data.OpenApi hiding (get)
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.Graph as Actions
import qualified Language.Mimsa.Actions.Helpers.Swaps as Actions
import qualified Language.Mimsa.Actions.Upgrade as Actions
import Language.Mimsa.Printer
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

-- /project/upgrade/

type UpgradeAPI =
  "upgrade" :> ReqBody '[JSON] UpgradeRequest
    :> JsonPost UpgradeResponse

data UpgradeRequest = UpgradeRequest
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

data UpgradeResponse = UpgradeResponse
  { upUpgradedDeps :: Map Text FromTo,
    upExpressionData :: ExpressionData,
    upProjectData :: ProjectData,
    upTestData :: TestData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

mapUpgradedDeps :: (Printer n) => Map ExprHash (n, ExprHash) -> Map Text FromTo
mapUpgradedDeps =
  M.fromList
    . fmap
      ( \(startHash, (ident, endHash)) ->
          (prettyPrint ident, FromTo startHash endHash)
      )
    . M.toList

upgrade ::
  MimsaEnvironment ->
  UpgradeRequest ->
  MimsaHandler UpgradeResponse
upgrade mimsaEnv (UpgradeRequest bindingName projectHash) =
  runMimsaHandlerT $ do
    let action = do
          (resolvedExpr, _, depUpdates) <-
            Actions.upgradeByName bindingName
          let (ResolvedExpression _ se _ _ swaps typedExpr input) = resolvedExpr
          typedNameExpr <- Actions.useSwaps swaps typedExpr
          gv <- Actions.graphExpression se
          let warnings = getWarnings resolvedExpr
          pure
            ( mapUpgradedDeps depUpdates,
              makeExpressionData se typedNameExpr gv input warnings
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
      Right (newProject, (upgradedDeps, ed)) -> do
        pd <- lift $ projectDataHandler mimsaEnv newProject
        returnMimsa $ UpgradeResponse upgradedDeps ed pd testData
