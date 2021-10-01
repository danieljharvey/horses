{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Server.Handlers
  ( ProjectData (..),
    fromActionM,
    eitherFromActionM,
    projectDataHandler,
    loadProjectHandler,
    saveExprHandler,
    saveFileHandler,
    findExprHandler,
    storeFromExprHashHandler,
    resolveStoreExpressionHandler,
    readStoreHandler,
    writeStoreHandler,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Monad.Except
import qualified Data.Aeson as JSON
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.OpenApi
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Shared as Actions
  ( getTypeMap,
    resolveStoreExpression,
  )
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.Persistence
import Language.Mimsa.Server.Helpers
import Language.Mimsa.Server.Types
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Servant

-----
-- Commonly used functionality, lifted into Servant's Handler type
-----

fromActionM ::
  MimsaEnvironment ->
  ProjectHash ->
  Actions.ActionM a ->
  Handler (Project Annotation, a)
fromActionM mimsaEnv projectHash action = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' projectHash
  case Actions.run project action of
    Left e -> throwError (to400Error e)
    Right (newProject, outcomes, a) -> do
      traverse_ (saveExprHandler mimsaEnv) (Actions.storeExpressionsFromOutcomes outcomes)
      -- TODO: return these as a list of Text to show up in a toast in app
      -- traverse_ replOutput (Actions.messagesFromOutcomes outcomes)
      traverse_ (saveFileHandler mimsaEnv) (Actions.writeFilesFromOutcomes outcomes)

      pure (newProject, a)

eitherFromActionM ::
  MimsaEnvironment ->
  ProjectHash ->
  Actions.ActionM a ->
  Handler (Either (Error Annotation) (Project Annotation, a))
eitherFromActionM mimsaEnv projectHash action = do
  store' <- readStoreHandler mimsaEnv
  project <- loadProjectHandler mimsaEnv store' projectHash
  case Actions.run project action of
    Left e -> pure (Left e)
    Right (newProject, outcomes, a) -> do
      traverse_ (saveExprHandler mimsaEnv) (Actions.storeExpressionsFromOutcomes outcomes)
      -- TODO: return these as a list of Text to show up in a toast in app
      -- traverse_ replOutput (Actions.messagesFromOutcomes outcomes)
      traverse_ (saveFileHandler mimsaEnv) (Actions.writeFilesFromOutcomes outcomes)

      pure (Right (newProject, a))

outputBindings :: Project a -> Map Name Text
outputBindings project =
  prettyPrint
    <$> getBindings
      ( getCurrentBindings
          (prjBindings project)
      )

outputTypeBindings :: Project a -> Map TyCon Text
outputTypeBindings project =
  prettyPrint
    <$> getTypeBindings
      (getCurrentTypeBindings (prjTypeBindings project))

data ProjectData = ProjectData
  { pdHash :: ProjectHash,
    pdBindings :: Map Name Text,
    pdTypeBindings :: Map TyCon Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

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
projectDataHandler ::
  MimsaEnvironment ->
  Project ann ->
  Handler ProjectData
projectDataHandler mimsaEnv env = do
  projHash <-
    handleMimsaM
      (mimsaConfig mimsaEnv)
      InternalError
      (saveProjectInStore env)
  pure $
    ProjectData
      projHash
      (outputBindings env)
      (outputTypeBindings env)

-- given a project hash, find the project
loadProjectHandler ::
  MimsaEnvironment ->
  Store Annotation ->
  ProjectHash ->
  Handler (Project Annotation)
loadProjectHandler mimsaEnv store' hash =
  handleMimsaM (mimsaConfig mimsaEnv) UserError (loadProjectFromHash store' hash)

saveExprHandler ::
  MimsaEnvironment ->
  StoreExpression ann ->
  Handler ExprHash
saveExprHandler mimsaEnv se =
  handleMimsaM (mimsaConfig mimsaEnv) InternalError (saveExpr se)

saveFileHandler ::
  MimsaEnvironment ->
  (Actions.SavePath, Actions.SaveFilename, Actions.SaveContents) ->
  Handler ()
saveFileHandler mimsaEnv saveInfo =
  handleMimsaM (mimsaConfig mimsaEnv) InternalError (saveFile saveInfo)

resolveStoreExpressionHandler ::
  Project Annotation ->
  StoreExpression Annotation ->
  Handler (ResolvedExpression Annotation)
resolveStoreExpressionHandler prj se = do
  typeMap <- handleEither InternalError (Actions.getTypeMap prj)
  handleEither UserError $
    Actions.resolveStoreExpression
      (prjStore prj)
      typeMap
      (prettyPrint (storeExpression se))
      se

findExprHandler ::
  Project Annotation ->
  ExprHash ->
  Handler (StoreExpression Annotation)
findExprHandler project exprHash' =
  handleEither InternalError $
    case lookupExprHash project exprHash' of
      Nothing -> Left ("Could not find exprhash!" :: Text)
      Just a -> Right a

storeFromExprHashHandler ::
  MimsaEnvironment ->
  ExprHash ->
  Handler (Store ())
storeFromExprHashHandler mimsaEnv exprHash =
  handleMimsaM
    (mimsaConfig mimsaEnv)
    UserError
    (recursiveLoadBoundExpressions mempty (S.singleton exprHash))
