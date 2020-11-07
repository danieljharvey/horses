{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project
  ( projectEndpoints,
    ProjectAPI,
  )
where

import Control.Monad.Except
import qualified Data.Aeson as JSON
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Generics
import Language.Mimsa.Actions (evaluateText)
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Servant

newtype EvaluateRequest
  = EvaluateRequest {code :: Text}
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON)

data EvaluateResponse
  = EvaluateResponse {prettyExpr :: Text, prettyType :: Text, prettyHash :: Text}
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON)

type ProjectAPI =
  "project" :> ("evaluate" :> ReqBody '[JSON] EvaluateRequest :> Post '[JSON] EvaluateResponse)

projectEndpoints :: Project Annotation -> Server ProjectAPI
projectEndpoints = evaluateExpression

evaluateExpression ::
  Project Annotation ->
  EvaluateRequest ->
  Handler EvaluateResponse
evaluateExpression project body = do
  case evaluateText project (code body) of
    Left e -> throwError (to500Error e)
    Right (ResolvedExpression mt se _ _ _) -> do
      let prettyExpr' = prettyPrint (storeExpression se)
          prettyType' = prettyPrint mt
          prettyHash' = prettyPrint (getStoreExpressionHash se)
      pure (EvaluateResponse prettyExpr' prettyType' prettyHash')

to500Error :: (Printer a) => a -> ServerError
to500Error a = err500 {errBody = buildMsg a}
  where
    buildMsg = encodeUtf8 . fromStrict . prettyPrint
