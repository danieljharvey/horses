{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.Evaluate
  ( evaluateExpression,
    EvaluateAPI,
  )
where

import qualified Data.Aeson as JSON
import Data.Swagger
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.Evaluate as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Server.ExpressionData
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.Project
import Servant

-- /project/evaluate/

type EvaluateAPI =
  "evaluate" :> ReqBody '[JSON] EvaluateRequest
    :> Post '[JSON] EvaluateResponse

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
  Handler EvaluateResponse
evaluateExpression mimsaEnv (EvaluateRequest code hash) = do
  expr <- parseHandler code
  (newProject, (mt, simpleExpr, se, gv)) <-
    fromActionM
      mimsaEnv
      hash
      (Actions.evaluate code expr)
  EvaluateResponse (prettyPrint simpleExpr)
    <$> expressionDataHandler newProject se mt gv
