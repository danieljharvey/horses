{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Project.Persistence
  ( loadProject,
    loadProjectFromHash,
    saveProject,
    saveProjectInStore,
    getCurrentBindings,
    getCurrentTypeBindings,
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
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store.Hashing
import Language.Mimsa.Store.Storage
import Language.Mimsa.Types.Error.StoreError
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

type PersistApp a = ExceptT StoreError IO a

storePath :: String
storePath = "./"

envPath :: String
envPath = storePath <> "environment.json"

hush :: Either IOError a -> Maybe a
hush (Right a) = pure a
hush _ = Nothing

getProjectFolder :: IO FilePath
getProjectFolder = getStoreFolder "projects"

getProjectPath :: ProjectHash -> IO FilePath
getProjectPath hash' = do
  folder <- getProjectFolder
  pure (folder <> show hash' <> ".json")

-- load environment.json and any hashed exprs mentioned in it
-- should probably consider loading the exprs lazily as required in future
loadProject :: (Monoid ann) => PersistApp (Project ann)
loadProject = do
  proj <- loadProject'
  pure $ proj $> mempty

loadProject' ::
  PersistApp (Project ())
loadProject' = do
  project' <- liftIO $ try $ LBS.readFile envPath
  case hush project' >>= JSON.decode of
    Just sp -> fetchProjectItems mempty sp -- we're starting from scratch with this one
    _ -> throwError $ CouldNotDecodeFile envPath

loadProjectFromHash :: (Monoid ann) => Store ann -> ProjectHash -> PersistApp (Project ann)
loadProjectFromHash store' hash = do
  let unitStore = store' $> ()
  proj <- loadProjectFromHash' unitStore hash
  pure $ proj $> mempty

loadProjectFromHash' :: Store () -> ProjectHash -> PersistApp (Project ())
loadProjectFromHash' store' hash = do
  path <- liftIO $ getProjectPath hash
  json <- liftIO $ try $ LBS.readFile path
  case hush json >>= JSON.decode of
    Just sp -> fetchProjectItems store' sp
    _ -> throwError $ CouldNotDecodeFile envPath

fetchProjectItems :: Store () -> SaveProject -> PersistApp (Project ())
fetchProjectItems existingStore sp = do
  store' <-
    recursiveLoadBoundExpressions
      existingStore
      (getItemsForAllVersions . projectBindings $ sp)
  typeStore' <-
    recursiveLoadBoundExpressions
      existingStore
      (getItemsForAllVersions . projectTypes $ sp)
  pure $ projectFromSaved (existingStore <> store' <> typeStore') sp

-- save project in local folder
saveProject :: Project ann -> PersistApp ProjectHash
saveProject p = saveProject' (p $> ())

saveProject' :: Project () -> PersistApp ProjectHash
saveProject' env = do
  let (jsonStr, _) = contentAndHash (projectToSaved env)
  liftIO $ LBS.writeFile envPath jsonStr
  saveProjectInStore' env

-- save project in store
saveProjectInStore :: Project ann -> PersistApp ProjectHash
saveProjectInStore p = saveProjectInStore' (p $> ())

saveProjectInStore' :: Project () -> PersistApp ProjectHash
saveProjectInStore' env = do
  let (jsonStr, hash) = contentAndHash (projectToSaved env)
  path <- liftIO $ getProjectPath hash
  liftIO $ LBS.writeFile path jsonStr
  pure hash

--

storeItems :: Store a -> Set ExprHash
storeItems (Store s) = S.fromList (M.keys s)

loadBoundExpressions ::
  Set ExprHash ->
  PersistApp (Store ())
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
  PersistApp (Store ())
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
