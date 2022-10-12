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
import qualified Language.Mimsa.Actions.Helpers.Parse as Actions
import qualified Language.Mimsa.Actions.Modules.Evaluate as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Servant
import Server.Handlers
import Server.Helpers.ExpressionData
import Server.MimsaHandler
import Server.Types

-- using https://github.com/haskell-servant/servant/blob/master/doc/cookbook/uverb/UVerb.lhs

type EvaluateAPI = EvaluateModule

evaluate :: MimsaEnvironment -> Server EvaluateAPI
evaluate =
  evaluateModuleExpression


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
          let se = StoreExpression exprResult mempty mempty mempty mempty
          pure $
            EvaluateModuleResponse
              (prettyPrint exprResult)
              (makeMinimalExpressionData se mt input)
    response <- lift $ eitherFromActionM mimsaEnv hash action
    case response of
      Left e -> throwMimsaError e
      Right (_, _, a) -> returnMimsa a
