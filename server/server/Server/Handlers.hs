{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Server.Handlers
  ( fromActionM,
    eitherFromActionM,
    projectDataHandler,
    loadProjectHandler,
    saveExprHandler,
    saveFileHandler,
    findExprHandler,
    projectFromModuleHandler,
    readStoreHandler,
    writeStoreHandler,
    readModuleStoreHandler,
    writeModuleStoreHandler,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.Functor
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Actions.Types
import Language.Mimsa.Core
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store
import Language.Mimsa.Store.Persistence
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Servant
import Server.Helpers
import Server.Helpers.ProjectData
import Server.Persistence
import Server.ServerConfig
import Server.Types

-----
-- Commonly used functionality, lifted into Servant's Handler type
-----

fromActionM ::
  MimsaEnvironment ->
  ProjectHash ->
  Actions.ActionM a ->
  Handler (Project Annotation, [ActionOutcome], a)
fromActionM mimsaEnv projectHash action = do
  eitherResp <- eitherFromActionM mimsaEnv projectHash action
  case eitherResp of
    Left e -> throwError (to400Error e)
    Right a -> pure a

eitherFromActionM ::
  MimsaEnvironment ->
  ProjectHash ->
  Actions.ActionM a ->
  Handler (Either (Error Annotation) (Project Annotation, [ActionOutcome], a))
eitherFromActionM mimsaEnv projectHash action = do
  project <- loadProjectHandler mimsaEnv projectHash
  case Actions.run project action of
    Left e -> pure (Left e)
    Right (newProject, outcomes, a) -> do
      traverse_ (saveExprHandler mimsaEnv) (Actions.storeExpressionsFromOutcomes outcomes)
      -- TODO: also return these as a list of Text to show up in a toast in app
      traverse_ (liftIO . T.putStrLn) (Actions.messagesFromOutcomes outcomes)
      traverse_ (saveFileHandler mimsaEnv) (Actions.writeFilesFromOutcomes outcomes)
      pure (Right (newProject, outcomes, a))

-- read the store from mutable var to stop repeated loading of exprs
readStoreHandler :: MimsaEnvironment -> Handler (Store Annotation)
readStoreHandler mimsaEnv = do
  liftIO $ STM.readTVarIO (mutableStore mimsaEnv)

-- write the store to mutable var to reduce file system access
writeStoreHandler :: MimsaEnvironment -> Store Annotation -> Handler ()
writeStoreHandler mimsaEnv store' = do
  liftIO $
    STM.atomically $
      STM.modifyTVar
        (mutableStore mimsaEnv)
        (<> store')

-- read the store from mutable var to stop repeated loading of exprs
readModuleStoreHandler :: MimsaEnvironment -> Handler (Map ModuleHash (Module Annotation))
readModuleStoreHandler mimsaEnv = do
  liftIO $ STM.readTVarIO (mutableModuleStore mimsaEnv)

-- write the store to mutable var to reduce file system access
writeModuleStoreHandler :: MimsaEnvironment -> Map ModuleHash (Module Annotation) -> Handler ()
writeModuleStoreHandler mimsaEnv moduleStore = do
  liftIO $
    STM.atomically $
      STM.modifyTVar
        (mutableModuleStore mimsaEnv)
        (<> moduleStore)

-- given a project hash, find the project
loadProjectHandler ::
  MimsaEnvironment ->
  ProjectHash ->
  Handler (Project Annotation)
loadProjectHandler mimsaEnv projectHash = do
  store' <- readStoreHandler mimsaEnv
  moduleStore <- readModuleStoreHandler mimsaEnv
  handleServerM (mimsaConfig mimsaEnv) UserError (loadProjectFromHash store' moduleStore projectHash)

saveExprHandler ::
  MimsaEnvironment ->
  StoreExpression ann ->
  Handler ExprHash
saveExprHandler mimsaEnv se = do
  let cfg = mimsaConfig mimsaEnv
  handleServerM @() cfg InternalError (saveExpr (scRootPath cfg) se)

saveFileHandler ::
  MimsaEnvironment ->
  (Actions.SavePath, Actions.SaveFilename, Actions.SaveContents) ->
  Handler ()
saveFileHandler mimsaEnv saveInfo = do
  let cfg = mimsaConfig mimsaEnv
  handleServerM @() cfg InternalError (saveFile (scRootPath cfg) saveInfo)

findExprHandler ::
  Project Annotation ->
  ExprHash ->
  Handler (StoreExpression Annotation)
findExprHandler project exprHash' =
  handleEither InternalError $
    case lookupExprHash project exprHash' of
      Nothing -> Left ("Could not find exprhash!" :: Text)
      Just a -> Right a

findModuleHandler ::
  Project Annotation ->
  ModuleHash ->
  Handler (Module Annotation)
findModuleHandler project modHash =
  handleEither InternalError $
    case lookupModuleHash project modHash of
      Nothing -> Left ("Could not find moduleHash!" :: Text)
      Just a -> Right a

-- given a moduleHash, load a project containing its dependents
projectFromModuleHandler ::
  MimsaEnvironment ->
  ModuleHash ->
  Handler (Module Annotation, ProjectData, Project Annotation)
projectFromModuleHandler mimsaEnv modHash = do
  -- load store with just items for module in
  modules <- storeFromModuleHashHandler mimsaEnv modHash
  -- create a project with this store
  let project = fromModuleStore modules $> mempty
  -- find the storeExpr we want in the store
  foundModule <- findModuleHandler project modHash
  -- save shit
  pd <- projectDataHandler mimsaEnv project
  -- cache our findings
  writeStoreHandler mimsaEnv (prjStore project)
  pure (foundModule, pd, project)

storeFromModuleHashHandler ::
  MimsaEnvironment ->
  ModuleHash ->
  Handler (Map ModuleHash (Module ()))
storeFromModuleHashHandler mimsaEnv modHash =
  let cfg = mimsaConfig mimsaEnv
   in handleServerM
        (mimsaConfig mimsaEnv)
        UserError
        (recursiveLoadModules (scRootPath cfg) mempty (S.singleton modHash))
