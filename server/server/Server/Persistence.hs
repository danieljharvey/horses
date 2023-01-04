{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Persistence
  ( loadProjectFromHash,
    saveProjectInStore,
  )
where

-- functions for Projects as opposed to the larger Store

import Control.Exception
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.Functor
import Data.Map.Strict (Map)
import Language.Mimsa.Core
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store.Hashing
import Language.Mimsa.Store.Persistence
import Language.Mimsa.Store.Storage
import Language.Mimsa.Types.Error.StoreError
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Server.ServerConfig
import System.Directory

getProjectFolder ::
  (MonadIO m, MonadReader ServerConfig m) =>
  m FilePath
getProjectFolder = do
  rootPath <- asks scRootPath
  getStoreFolder rootPath "projects"

getProjectPath :: (MonadIO m, MonadReader ServerConfig m) => ProjectHash -> m FilePath
getProjectPath hash' = do
  folder <- getProjectFolder
  pure (folder <> getProjectFilename hash')

getProjectFilename :: ProjectHash -> FilePath
getProjectFilename hash' = show hash' <> ".json"

loadProjectFromHash ::
  ( Monoid ann,
    MonadIO m,
    MonadError StoreError m,
    MonadLogger m,
    MonadReader ServerConfig m
  ) =>
  Store ann ->
  Map ModuleHash (Module ann) ->
  ProjectHash ->
  m (Project ann)
loadProjectFromHash store' moduleStore hash = do
  let unitStore = store' $> ()
      unitModuleStore = fmap void moduleStore
  proj <- loadProjectFromHash' unitStore unitModuleStore hash
  pure $ proj $> mempty

loadProjectFromHash' ::
  (MonadIO m, MonadError StoreError m, MonadLogger m, MonadReader ServerConfig m) =>
  Store () ->
  Map ModuleHash (Module ()) ->
  ProjectHash ->
  m (Project ())
loadProjectFromHash' store' moduleStore hash = do
  rootPath <- asks scRootPath
  path <- getProjectPath hash
  json <- liftIO $ try $ LBS.readFile path
  case json of
    Left (_ :: IOError) ->
      throwError $
        CouldNotReadFilePath ProjectFile (getProjectFilename hash)
    Right json' -> case JSON.decode json' of
      Just sp -> fetchProjectItems rootPath store' moduleStore sp
      _ -> throwError $ CouldNotDecodeFile (getProjectFilename hash)

-- save project in store
saveProjectInStore ::
  ( MonadIO m,
    MonadError StoreError m,
    MonadLogger m,
    MonadReader ServerConfig m
  ) =>
  Project ann ->
  m ProjectHash
saveProjectInStore p = saveProjectInStore' (p $> ())

saveProjectInStore' ::
  ( MonadIO m,
    MonadError StoreError m,
    MonadLogger m,
    MonadReader ServerConfig m
  ) =>
  Project () ->
  m ProjectHash
saveProjectInStore' env = do
  let (jsonStr, hash) = contentAndHash (projectToSaved env)
  path <- getProjectPath hash
  exists <- liftIO $ doesFileExist path
  if exists
    then do
      logDebugN $ "Project file for " <> prettyPrint hash <> " already exists"
      pure hash
    else do
      logDebugN $ "Saved project for " <> prettyPrint hash
      success <- liftIO $ try $ LBS.writeFile path jsonStr
      case success of
        Left (_ :: IOError) ->
          throwError (CouldNotWriteFilePath ProjectFile (getProjectFilename hash))
        Right _ -> pure hash
