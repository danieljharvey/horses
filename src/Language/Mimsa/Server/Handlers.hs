{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Server.Handlers
  ( ProjectData (..),
    projectDataHandler,
    ExpressionData (..),
    expressionDataHandler,
    loadProjectHandler,
    evaluateTextHandler,
    saveExprHandler,
    interpretHandler,
    findExprHandler,
    resolveStoreExpressionHandler,
    readStoreHandler,
    writeStoreHandler,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Monad.Except
import qualified Data.Aeson as JSON
import Data.Map (Map)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Actions (evaluateText, resolveStoreExpression)
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Server.Helpers
import Language.Mimsa.Server.Types
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker
import Servant

-----
-- Commonly used functionality, lifted into Servant's Handler type
-----

outputBindings :: Project a -> Map Name Text
outputBindings project =
  prettyPrint
    <$> getBindings
      ( getCurrentBindings
          (bindings project)
      )

outputTypeBindings :: Project a -> Map TyCon Text
outputTypeBindings project =
  prettyPrint
    <$> getTypeBindings
      (getCurrentTypeBindings (typeBindings project))

data ProjectData = ProjectData
  { pdHash :: ProjectHash,
    pdBindings :: Map Name Text,
    pdTypeBindings :: Map TyCon Text
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

-- read the store from mutable var to stop repeated loading of exprs
readStoreHandler :: MimsaEnvironment -> Handler (Store Annotation)
readStoreHandler mimsaEnv = do
  liftIO $ STM.atomically $ STM.readTVar (mutableStore mimsaEnv)

writeStoreHandler :: MimsaEnvironment -> Store Annotation -> Handler ()
writeStoreHandler mimsaEnv store' = do
  liftIO $
    STM.atomically $
      STM.modifyTVar
        (mutableStore mimsaEnv)
        (<> store')

-- given a new Project, save it and return the hash and bindings
projectDataHandler :: MimsaEnvironment -> Project ann -> Handler ProjectData
projectDataHandler mimsaEnv env = do
  projHash <- handleExceptT InternalError (saveProjectInStore (mimsaConfig mimsaEnv) env)
  pure $
    ProjectData
      projHash
      (outputBindings env)
      (outputTypeBindings env)

data ExpressionData = ExpressionData
  { edHash :: Text,
    edPretty :: Text,
    edType :: Text,
    edBindings :: Map Name Text,
    edTypeBindings :: Map TyCon Text
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, ToSchema)

expressionDataHandler :: MimsaEnvironment -> StoreExpression Annotation -> MonoType -> Handler ExpressionData
expressionDataHandler mimsaEnv se mt = do
  _ <- saveExprHandler mimsaEnv se
  pure $
    ExpressionData
      (prettyPrint (getStoreExpressionHash se))
      (prettyPrint (storeExpression se))
      (prettyPrint mt)
      (prettyPrint <$> getBindings (storeBindings se))
      (prettyPrint <$> getTypeBindings (storeTypeBindings se))

-- given a project hash, find the project
loadProjectHandler ::
  MimsaEnvironment ->
  Store Annotation ->
  ProjectHash ->
  Handler (Project Annotation)
loadProjectHandler mimsaEnv store' hash =
  handleExceptT UserError $ loadProjectFromHash (mimsaConfig mimsaEnv) store' hash

evaluateTextHandler ::
  Project Annotation ->
  Text ->
  Handler (ResolvedExpression Annotation)
evaluateTextHandler project code = handleEither UserError (evaluateText project code)

saveExprHandler :: MimsaEnvironment -> StoreExpression ann -> Handler ExprHash
saveExprHandler mimsaEnv se =
  handleExceptT InternalError (saveExpr (mimsaConfig mimsaEnv) se)

interpretHandler ::
  Scope Annotation ->
  Swaps ->
  Expr Variable Annotation ->
  Handler (Expr Variable Annotation)
interpretHandler scope' swaps' expr' =
  handleEither InternalError (interpret scope' swaps' expr')

resolveStoreExpressionHandler ::
  Store Annotation ->
  StoreExpression Annotation ->
  Handler (ResolvedExpression Annotation)
resolveStoreExpressionHandler store' se =
  handleEither UserError $ resolveStoreExpression store' "" se

findExprHandler ::
  Project Annotation ->
  ExprHash ->
  Handler (StoreExpression Annotation)
findExprHandler project exprHash' =
  handleEither InternalError $
    case lookupExprHash project exprHash' of
      Nothing -> Left ("Could not find exprhash!" :: Text)
      Just a -> Right a
