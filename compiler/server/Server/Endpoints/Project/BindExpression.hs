{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Server.Endpoints.Project.BindExpression
  ( bindExpression,
    BindExpression,
  )
where

import Control.Monad.Trans.Class
import qualified Data.Aeson as JSON
import Data.Bifunctor
import qualified Data.Map.Strict as M
import Data.OpenApi
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.CanOptimise as Actions
import qualified Language.Mimsa.Actions.Helpers.Parse as Actions
import Language.Mimsa.Transform.Warnings
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Servant
import Server.Handlers
import Server.Helpers.ExpressionData
import Server.Helpers.TestData
import Server.MimsaHandler
import Server.Types

------

type BindExpression =
  "bind"
    :> ReqBody '[JSON] BindExpressionRequest
    :> JsonPost BindExpressionResponse

data BindExpressionRequest = BindExpressionRequest
  { beProjectHash :: ProjectHash,
    beBindingName :: Name,
    beExpression :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, ToSchema)

data BindExpressionResponse = BindExpressionResponse
  { beProjectData :: ProjectData,
    beExpressionData :: ExpressionData,
    beTestData :: TestData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

bindExpression ::
  MimsaEnvironment ->
  BindExpressionRequest ->
  MimsaHandler BindExpressionResponse
bindExpression mimsaEnv (BindExpressionRequest projectHash name' input) = runMimsaHandlerT $ do
  let action = do
        expr <- Actions.parseExpr input
        (_, _, resolved@(ResolvedExpression _ se _ typedExpr input')) <-
          Actions.bindExpression expr name' input
        let typedNameExpr = first fst typedExpr
        let warnings = getWarnings resolved
        canOptimise <- Actions.canOptimise se
        pure $ makeExpressionData se typedNameExpr input' warnings canOptimise
  project <- lift $ loadProjectHandler mimsaEnv projectHash
  response <-
    lift $ eitherFromActionM mimsaEnv projectHash action
  tests <-
    lift $
      runTestsHandler
        mimsaEnv
        project
        (M.elems $ prjTests project)
  let testData = makeTestData project tests
  case response of
    Right (newProject, _, ed) -> do
      pd <- lift $ projectDataHandler mimsaEnv newProject
      returnMimsa $ BindExpressionResponse pd ed testData
    Left e -> throwMimsaError e
