{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.BindExpression
  ( bindExpression,
    BindExpression,
  )
where

import qualified Data.Aeson as JSON
import Data.Bifunctor (first)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.BindExpression as Actions
import Language.Mimsa.Parser (parseExprAndFormatError)
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Helpers
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Servant

------

type BindExpression =
  "bind"
    :> ReqBody '[JSON] BindExpressionRequest
    :> Post '[JSON] BindExpressionResponse

data BindExpressionRequest = BindExpressionRequest
  { beProjectHash :: ProjectHash,
    beBindingName :: Name,
    beExpression :: Text
  }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, ToSchema)

data BindExpressionResponse = BindExpressionResponse
  { beProjectData :: ProjectData,
    beExpressionData :: ExpressionData,
    beUpdatedTestsCount :: Int
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

parseExpr :: Text -> Either (Error Annotation) (Expr Name Annotation)
parseExpr input = first ParseError (parseExprAndFormatError input)

bindExpression ::
  MimsaEnvironment ->
  BindExpressionRequest ->
  Handler BindExpressionResponse
bindExpression mimsaEnv (BindExpressionRequest hash name' input) = do
  expr <- handleEither UserError (parseExpr input)
  (newProject, (_, numTests, ResolvedExpression mt se _ _ _)) <-
    fromActionM
      mimsaEnv
      hash
      (Actions.bindExpression expr name' input)
  pd <- projectDataHandler mimsaEnv newProject
  ed <- expressionDataHandler newProject se mt
  pure $
    BindExpressionResponse
      pd
      ed
      numTests
