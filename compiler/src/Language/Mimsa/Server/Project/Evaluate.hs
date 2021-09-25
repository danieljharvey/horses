{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.Evaluate
  ( evaluateExpression,
    EvaluateAPI,
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.Evaluate as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Server.ExpressionData
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.Project
import Servant

-- using https://github.com/haskell-servant/servant/blob/master/doc/cookbook/uverb/UVerb.lhs

-- /project/evaluate/

type EvaluateAPI =
  "evaluate" :> ReqBody '[JSON] EvaluateRequest
    :> UVerb
         'POST
         '[JSON]
         '[ EvaluateResponse,
            WithStatus 400 EvaluateError
          ]

data EvaluateError = EvaluateError
  { eeText :: Text,
    eeErrorSpans :: [Text]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

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

-- set return type for regular response
instance HasStatus EvaluateResponse where
  type StatusOf EvaluateResponse = 200

evaluateExpression ::
  MimsaEnvironment ->
  EvaluateRequest ->
  Handler
    ( Union
        '[ EvaluateResponse,
           WithStatus 400 EvaluateError
         ]
    )
evaluateExpression mimsaEnv (EvaluateRequest code hash) = do
  expr <- parseHandler code
  (newProject, (_, simpleExpr, se, gv, typedExpr, input)) <-
    fromActionM
      mimsaEnv
      hash
      (Actions.evaluate code expr)
  success <-
    EvaluateResponse (prettyPrint simpleExpr)
      <$> expressionDataHandler newProject se typedExpr gv input
  respond success
