{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Project.Persistence
  ( loadProject,
    loadProjectFromHash,
    saveProject,
    saveProjectInStore,
    getCurrentBindings,
    getCurrentTypeBindings,
    recursiveLoadBoundExpressions,
  )
where

-- functions for Projects as opposed to the larger Store

import Control.Exception
import Control.Monad.Except
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.Functor
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Monad
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store.Hashing
import Language.Mimsa.Store.Storage
import Language.Mimsa.Types.Error.StoreError
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

storePath :: String
storePath = "./"

envPath :: String
envPath = storePath <> "environment.json"

getProjectFolder :: MimsaM e FilePath
getProjectFolder = getStoreFolder "projects"

getProjectPath :: ProjectHash -> MimsaM e FilePath
getProjectPath hash' = do
  folder <- getProjectFolder
  pure (folder <> getProjectFilename hash')

getProjectFilename :: ProjectHash -> FilePath
getProjectFilename hash' = show hash' <> ".json"

-- load environment.json and any hashed exprs mentioned in it
-- should probably consider loading the exprs lazily as required in future
loadProject :: (Monoid ann) => MimsaM StoreError (Project ann)
loadProject = do
  proj <- loadProject'
  pure $ proj $> mempty

loadProject' ::
  MimsaM StoreError (Project ())
loadProject' = do
  project' <- liftIO $ try $ LBS.readFile envPath
  case project' of
    Left (_ :: IOError) -> throwError (CouldNotReadFilePath envPath)
    Right json' ->
      case JSON.decode json' of
        Just sp -> fetchProjectItems mempty sp -- we're starting from scratch with this one
        _ -> throwError $ CouldNotDecodeFile envPath

loadProjectFromHash :: (Monoid ann) => Store ann -> ProjectHash -> MimsaM StoreError (Project ann)
loadProjectFromHash store' hash = do
  let unitStore = store' $> ()
  proj <- loadProjectFromHash' unitStore hash
  pure $ proj $> mempty

loadProjectFromHash' ::
  Store () ->
  ProjectHash ->
  MimsaM StoreError (Project ())
loadProjectFromHash' store' hash = do
  path <- getProjectPath hash
  json <- liftIO $ try $ LBS.readFile path
  case json of
    Left (_ :: IOError) -> throwError $ CouldNotReadFilePath (getProjectFilename hash)
    Right json' -> case JSON.decode json' of
      Just sp -> fetchProjectItems store' sp
      _ -> throwError $ CouldNotDecodeFile (getProjectFilename hash)

fetchProjectItems ::
  Store () ->
  SaveProject ->
  MimsaM StoreError (Project ())
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
      (M.keysSet $ projectTests sp)
  pure $
    projectFromSaved
      ( existingStore <> store' <> typeStore'
          <> testStore
      )
      sp

-- save project in local folder
saveProject ::
  Project ann ->
  MimsaM StoreError ProjectHash
saveProject p = saveProject' (p $> ())

saveProject' ::
  Project () ->
  MimsaM StoreError ProjectHash
saveProject' env = do
  let (jsonStr, _) = contentAndHash (projectToSaved env)
  success <- liftIO $ try $ LBS.writeFile envPath jsonStr
  case success of
    Left (_ :: IOError) ->
      throwError (CouldNotWriteFilePath envPath)
    Right _ -> saveProjectInStore' env

-- save project in store
saveProjectInStore ::
  Project ann ->
  MimsaM StoreError ProjectHash
saveProjectInStore p = saveProjectInStore' (p $> ())

saveProjectInStore' ::
  Project () ->
  MimsaM StoreError ProjectHash
saveProjectInStore' env = do
  let (jsonStr, hash) = contentAndHash (projectToSaved env)
  path <- getProjectPath hash
  success <- liftIO $ try $ LBS.writeFile path jsonStr
  case success of
    Left (_ :: IOError) ->
      throwError (CouldNotWriteFilePath (getProjectFilename hash))
    Right _ -> pure hash

--

storeItems :: Store a -> Set ExprHash
storeItems (Store s) = S.fromList (M.keys s)

loadBoundExpressions ::
  Set ExprHash ->
  MimsaM StoreError (Store ())
loadBoundExpressions hashes = do
  items' <-
    traverse
      ( \hash -> do
          item <- findExpr hash
          pure (hash, item)
      )
      (S.toList hashes)
  pure
    (Store (M.fromList items'))

recursiveLoadBoundExpressions ::
  Store () ->
  Set ExprHash ->
  MimsaM StoreError (Store ())
recursiveLoadBoundExpressions existingStore hashes = do
  newStore <- loadBoundExpressions (S.difference hashes (storeItems existingStore))
  let newHashes =
        S.difference
          ( S.unions $
              getDependencyHashes <$> M.elems (getStore newStore)
          )
          hashes
  if S.null newHashes
    then pure (existingStore <> newStore)
    else do
      moreStore <- recursiveLoadBoundExpressions (existingStore <> newStore) newHashes
      pure (existingStore <> newStore <> moreStore)

--
