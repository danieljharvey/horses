{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Project
  ( projectEndpoints,
    ProjectAPI,
  )
where

import Control.Monad.Except
import qualified Data.Aeson as JSON
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Generics
import Language.Mimsa.Actions (evaluateText, resolveStoreExpression)
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Servant

-- the Project endpoints output data in a way that front ends would be
-- interested

-- exprHashes should be strings to stop JS getting lost

type ProjectAPI =
  "project" :> (EvaluateAPI :<|> ListBindings :<|> GetExpression)

projectEndpoints :: Project Annotation -> Server ProjectAPI
projectEndpoints prj =
  evaluateExpression prj
    :<|> listBindings prj
    :<|> getExpression prj

-- /project/evaluate/

type EvaluateAPI =
  "evaluate" :> ReqBody '[JSON] EvaluateRequest
    :> Post '[JSON] EvaluateResponse

newtype EvaluateRequest
  = EvaluateRequest {code :: Text}
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON)

data EvaluateResponse
  = EvaluateResponse {prettyExpr :: Text, prettyType :: Text, prettyHash :: Text}
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON)

evaluateExpression ::
  Project Annotation ->
  EvaluateRequest ->
  Handler EvaluateResponse
evaluateExpression project body = do
  case evaluateText project (code body) of
    Left e -> throwError (to400Error e)
    Right (ResolvedExpression mt se _ _ _) -> do
      let prettyExpr' = prettyPrint (storeExpression se)
          prettyType' = prettyPrint mt
          prettyHash' = prettyPrint (getStoreExpressionHash se)
      pure (EvaluateResponse prettyExpr' prettyType' prettyHash')

-- /project/bindings/

type ListBindings =
  "bindings"
    :> Get '[JSON] BindingsResponse

data BindingsResponse
  = BindingsResponse
      { projectBindings :: Map Name Text,
        projectTypeBindings :: Map TyCon Text
      }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON)

listBindings ::
  Project Annotation ->
  Handler BindingsResponse
listBindings project = pure $ BindingsResponse values types
  where
    values =
      prettyPrint
        <$> getBindings
          ( getCurrentBindings
              (bindings project)
          )
    types =
      prettyPrint
        <$> getTypeBindings
          (getCurrentTypeBindings (typeBindings project))

-- /project/expression/

type GetExpression =
  "expression" :> Capture "exprHash" ExprHash
    :> Get '[JSON] ExpressionResponse

data ExpressionResponse
  = ExpressionResponse
      { exprValue :: Text,
        exprType :: Text,
        exprBindings :: Map Name Text,
        exprTypeBindings :: Map TyCon Text
      }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON)

getExpression :: Project Annotation -> ExprHash -> Handler ExpressionResponse
getExpression project exprHash' = case lookupExprHash project exprHash' of
  Nothing -> throwError $ err500 {errBody = "Could not find exprhash!"}
  Just se -> do
    case resolveStoreExpression (store project) "" se of
      Left e -> throwError $ to400Error e
      Right (ResolvedExpression mt _ _ _ _) -> do
        let prettyExpr' = prettyPrint (storeExpression se)
            prettyType' = prettyPrint mt
            prettyBindings = prettyPrint <$> getBindings (storeBindings se)
            prettyTypeBindings = prettyPrint <$> getTypeBindings (storeTypeBindings se)
        pure
          ( ExpressionResponse
              prettyExpr'
              prettyType'
              prettyBindings
              prettyTypeBindings
          )

to400Error :: (Printer a) => a -> ServerError
to400Error a = err400 {errBody = buildMsg a}
  where
    buildMsg = encodeUtf8 . fromStrict . prettyPrint
