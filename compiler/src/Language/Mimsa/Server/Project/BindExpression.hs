{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project.BindExpression
  ( bindExpression,
    BindExpression,
  )
where

import Control.Monad.Trans.Class
import qualified Data.Aeson as JSON
import Data.OpenApi
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.Parse as Actions
import qualified Language.Mimsa.Actions.Helpers.Swaps as Actions
import Language.Mimsa.Server.Handlers
import Language.Mimsa.Server.Helpers.ExpressionData
import Language.Mimsa.Server.MimsaHandler
import Language.Mimsa.Server.Types
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Servant

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
    beExpressionData :: ExpressionData
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

bindExpression ::
  MimsaEnvironment ->
  BindExpressionRequest ->
  MimsaHandler BindExpressionResponse
bindExpression mimsaEnv (BindExpressionRequest hash name' input) = runMimsaHandlerT $ do
  let action = do
        expr <- Actions.parseExpr input
        (_, _, ResolvedExpression _ se _ _ swaps typedExpr input', gv) <-
          Actions.bindExpression expr name' input
        typedNameExpr <- Actions.useSwaps swaps typedExpr
        expressionData se typedNameExpr gv input'
  response <-
    lift $ eitherFromActionM mimsaEnv hash action
  case response of
    Right (newProject, ed) -> do
      pd <- lift $ projectDataHandler mimsaEnv newProject
      returnMimsa $ BindExpressionResponse pd ed
    Left e -> throwMimsaError e
