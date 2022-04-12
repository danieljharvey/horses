{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Persistence
  ( loadProjectFromHash,
    saveProjectInStore,
    recursiveLoadBoundExpressions,
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
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store.Hashing
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
  ProjectHash ->
  m (Project ann)
loadProjectFromHash store' hash = do
  let unitStore = store' $> ()
  proj <- loadProjectFromHash' unitStore hash
  pure $ proj $> mempty

loadProjectFromHash' ::
  (MonadIO m, MonadError StoreError m, MonadLogger m, MonadReader ServerConfig m) =>
  Store () ->
  ProjectHash ->
  m (Project ())
loadProjectFromHash' store' hash = do
  path <- getProjectPath hash
  json <- liftIO $ try $ LBS.readFile path
  case json of
    Left (_ :: IOError) ->
      throwError $
        CouldNotReadFilePath ProjectFile (getProjectFilename hash)
    Right json' -> case JSON.decode json' of
      Just sp -> fetchProjectItems store' sp
      _ -> throwError $ CouldNotDecodeFile (getProjectFilename hash)

fetchProjectItems ::
  (MonadIO m, MonadError StoreError m, MonadReader ServerConfig m, MonadLogger m) =>
  Store () ->
  SaveProject ->
  m (Project ())
fetchProjectItems existingStore sp = do
  store' <-
    recursiveLoadBoundExpressions
      existingStore
      (getItemsForAllVersions . projectBindings $ sp)
  typeStore' <-
    recursiveLoadBoundExpressions
      existingStore
      (getItemsForAllVersions . projectTypes $ sp)
  testStore <-
    recursiveLoadBoundExpressions
      existingStore
      ( M.keysSet
          ( projectUnitTests sp
          )
          <> M.keysSet (projectPropertyTests sp)
      )
  pure $
    projectFromSaved
      ( existingStore <> store' <> typeStore'
          <> testStore
      )
      sp

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

--

storeItems :: Store a -> Set ExprHash
storeItems (Store s) = S.fromList (M.keys s)

loadBoundExpressions ::
  (MonadIO m, MonadError StoreError m, MonadReader ServerConfig m, MonadLogger m) =>
  Set ExprHash ->
  m (Store ())
loadBoundExpressions hashes = do
  rootPath <- asks scRootPath
  items' <-
    traverse
      ( \hash -> do
          item <- findExpr rootPath hash
          pure (hash, item)
      )
      (S.toList hashes)
  pure
    (Store (M.fromList items'))

recursiveLoadBoundExpressions ::
  (MonadIO m, MonadError StoreError m, MonadReader ServerConfig m, MonadLogger m) =>
  Store () ->
  Set ExprHash ->
  m (Store ())
recursiveLoadBoundExpressions existingStore hashes = do
  newStore <-
    loadBoundExpressions
      (S.difference hashes (storeItems existingStore))
  let newHashes =
        S.difference
          ( S.unions $
              getDependencyHashes <$> M.elems (getStore newStore)
          )
          hashes
  if S.null newHashes
    then pure (existingStore <> newStore)
    else do
      moreStore <-
        recursiveLoadBoundExpressions
          (existingStore <> newStore)
          newHashes
      pure (existingStore <> newStore <> moreStore)

--
