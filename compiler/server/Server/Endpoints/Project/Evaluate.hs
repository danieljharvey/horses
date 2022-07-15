{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Server.Endpoints.Project.Evaluate
  ( evaluate,
    EvaluateAPI,
  )
where

import Control.Monad.Except
import qualified Data.Aeson as JSON
import Data.OpenApi hiding (Server, get)
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Graph as Actions
import qualified Language.Mimsa.Actions.Helpers.CheckStoreExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.Parse as Actions
import qualified Language.Mimsa.Actions.Modules.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Transform.Warnings
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Servant
import Server.Handlers
import Server.Helpers.ExpressionData
import Server.MimsaHandler
import Server.Types

-- using https://github.com/haskell-servant/servant/blob/master/doc/cookbook/uverb/UVerb.lhs

type EvaluateAPI = EvaluateSE :<|> EvaluateModule

evaluate :: MimsaEnvironment -> Server EvaluateAPI
evaluate mimsaEnv =
  evaluateExpression mimsaEnv :<|> evaluateModuleExpression mimsaEnv

-- /project/evaluate/

type EvaluateSE =
  "evaluate"
    :> ReqBody '[JSON] EvaluateRequest
    :> JsonPost EvaluateResponse

data EvaluateRequest = EvaluateRequest
  { erCode :: Text,
    erProjectHash :: ProjectHash
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, ToSchema)

data EvaluateResponse = EvaluateResponse
  { erResult :: Text,
    erExpressionData :: ExpressionData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

evaluateExpression ::
  MimsaEnvironment ->
  EvaluateRequest ->
  MimsaHandler EvaluateResponse
evaluateExpression mimsaEnv (EvaluateRequest code hash) =
  runMimsaHandlerT $ do
    let action = do
          expr <- Actions.parseExpr code
          (_, simpleExpr, se, typedExpr, input) <-
            Actions.evaluate code expr
          project <- Actions.getProject
          gv <- Actions.graphExpression se
          res <- Actions.checkStoreExpression input project se
          let warnings = getWarnings res
          pure $
            EvaluateResponse
              (prettyPrint simpleExpr)
              (makeExpressionData se typedExpr gv input warnings False)
    response <- lift $ eitherFromActionM mimsaEnv hash action
    case response of
      Left e -> throwMimsaError e
      Right (_, _, a) -> returnMimsa a

-- /project/evaluate-module/

type EvaluateModule =
  "evaluate-module"
    :> ReqBody '[JSON] EvaluateModuleRequest
    :> JsonPost EvaluateModuleResponse

data EvaluateModuleRequest = EvaluateModuleRequest
  { emrCode :: Text,
    emrProjectHash :: ProjectHash
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, ToSchema)

data EvaluateModuleResponse = EvaluateModuleResponse
  { emrResult :: Text,
    emrExpressionData :: ExpressionData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

evaluateModuleExpression ::
  MimsaEnvironment ->
  EvaluateModuleRequest ->
  MimsaHandler EvaluateModuleResponse
evaluateModuleExpression mimsaEnv (EvaluateModuleRequest input hash) =
  runMimsaHandlerT $ do
    let action = do
          expr <- Actions.parseExpr input
          (mt, exprResult, _newModule) <-
            Actions.evaluateModule expr mempty
          let se = StoreExpression exprResult mempty mempty mempty
          pure $
            EvaluateModuleResponse
              (prettyPrint exprResult)
              (makeMinimalExpressionData se mt input False)
    response <- lift $ eitherFromActionM mimsaEnv hash action
    case response of
      Left e -> throwMimsaError e
      Right (_, _, a) -> returnMimsa a
