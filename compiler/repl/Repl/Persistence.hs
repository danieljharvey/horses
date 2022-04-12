{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repl.Persistence
  ( loadProject,
    saveProject,
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
import Language.Mimsa.Types.Store.RootPath
import Repl.Types
import System.Directory

projectFilePath :: RootPath -> String
projectFilePath (RootPath rp) = rp <> "/mimsa.json"

getProjectFolder ::
  (MonadIO m, MonadReader ReplConfig m) =>
  m FilePath
getProjectFolder = do
  rootPath <- asks rcRootPath
  getStoreFolder rootPath "projects"

getProjectPath :: (MonadIO m, MonadReader ReplConfig m) => ProjectHash -> m FilePath
getProjectPath hash' = do
  folder <- getProjectFolder
  pure (folder <> getProjectFilename hash')

getProjectFilename :: ProjectHash -> FilePath
getProjectFilename hash' = show hash' <> ".json"

-- load environment.json and any hashed exprs mentioned in it
-- should probably consider loading the exprs lazily as required in future
loadProject ::
  (MonadIO m, MonadError StoreError m, Monoid ann, MonadReader ReplConfig m, MonadLogger m) =>
  m (Project ann)
loadProject = do
  proj <- loadProject'
  pure $ proj $> mempty

loadProject' ::
  (MonadIO m, MonadError StoreError m, MonadLogger m, MonadReader ReplConfig m) =>
  m (Project ())
loadProject' = do
  rootPath <- asks rcRootPath
  project' <- liftIO $ try $ LBS.readFile (projectFilePath rootPath)
  case project' of
    Left (_ :: IOError) -> throwError (CouldNotReadFilePath ProjectFile (projectFilePath rootPath))
    Right json' ->
      case JSON.decode json' of
        Just sp -> fetchProjectItems mempty sp -- we're starting from scratch with this one
        _ -> throwError $ CouldNotDecodeFile (projectFilePath rootPath)

fetchProjectItems ::
  (MonadIO m, MonadError StoreError m, MonadReader ReplConfig m, MonadLogger m) =>
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

-- save project in local folder
saveProject ::
  (MonadIO m, MonadError StoreError m, MonadLogger m, MonadReader ReplConfig m) =>
  Project ann ->
  m ProjectHash
saveProject p = saveProject' (p $> ())

saveProject' ::
  (MonadIO m, MonadError StoreError m, MonadLogger m, MonadReader ReplConfig m) =>
  Project () ->
  m ProjectHash
saveProject' env = do
  rootPath <- asks rcRootPath
  let (jsonStr, _) = contentAndHash (projectToSaved env)
  success <- liftIO $ try $ LBS.writeFile (projectFilePath rootPath) jsonStr
  case success of
    Left (_ :: IOError) ->
      throwError (CouldNotWriteFilePath ProjectFile (projectFilePath rootPath))
    Right _ -> saveProjectInStore' env

saveProjectInStore' ::
  ( MonadIO m,
    MonadError StoreError m,
    MonadLogger m,
    MonadReader ReplConfig m
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
  (MonadIO m, MonadError StoreError m, MonadReader ReplConfig m, MonadLogger m) =>
  Set ExprHash ->
  m (Store ())
loadBoundExpressions hashes = do
  rootPath <- asks rcRootPath
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
  (MonadIO m, MonadError StoreError m, MonadReader ReplConfig m, MonadLogger m) =>
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
