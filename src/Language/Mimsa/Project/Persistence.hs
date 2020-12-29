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
import Language.Mimsa.Server.EnvVars
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

getProjectFolder :: MimsaConfig -> IO FilePath
getProjectFolder cfg = getStoreFolder cfg "projects"

getProjectPath :: MimsaConfig -> ProjectHash -> IO FilePath
getProjectPath cfg hash' = do
  folder <- getProjectFolder cfg
  pure (folder <> show hash' <> ".json")

-- load environment.json and any hashed exprs mentioned in it
-- should probably consider loading the exprs lazily as required in future
loadProject :: (Monoid ann) => MimsaConfig -> PersistApp (Project ann)
loadProject cfg = do
  proj <- loadProject' cfg
  pure $ proj $> mempty

loadProject' ::
  MimsaConfig ->
  PersistApp (Project ())
loadProject' cfg = do
  project' <- liftIO $ try $ LBS.readFile envPath
  case hush project' >>= JSON.decode of
    Just sp -> fetchProjectItems cfg mempty sp -- we're starting from scratch with this one
    _ -> throwError $ CouldNotDecodeFile envPath

loadProjectFromHash :: (Monoid ann) => MimsaConfig -> Store ann -> ProjectHash -> PersistApp (Project ann)
loadProjectFromHash cfg store' hash = do
  let unitStore = store' $> ()
  proj <- loadProjectFromHash' cfg unitStore hash
  pure $ proj $> mempty

loadProjectFromHash' :: MimsaConfig -> Store () -> ProjectHash -> PersistApp (Project ())
loadProjectFromHash' cfg store' hash = do
  path <- liftIO $ getProjectPath cfg hash
  json <- liftIO $ try $ LBS.readFile path
  case hush json >>= JSON.decode of
    Just sp -> fetchProjectItems cfg store' sp
    _ -> throwError $ CouldNotDecodeFile envPath

fetchProjectItems :: MimsaConfig -> Store () -> SaveProject -> PersistApp (Project ())
fetchProjectItems cfg existingStore sp = do
  store' <-
    recursiveLoadBoundExpressions
      cfg
      existingStore
      (getItemsForAllVersions . projectBindings $ sp)
  typeStore' <-
    recursiveLoadBoundExpressions
      cfg
      existingStore
      (getItemsForAllVersions . projectTypes $ sp)
  pure $ projectFromSaved (existingStore <> store' <> typeStore') sp

-- save project in local folder
saveProject :: MimsaConfig -> Project ann -> PersistApp ProjectHash
saveProject cfg p = saveProject' cfg (p $> ())

saveProject' :: MimsaConfig -> Project () -> PersistApp ProjectHash
saveProject' cfg env = do
  let (jsonStr, _) = contentAndHash (projectToSaved env)
  liftIO $ LBS.writeFile envPath jsonStr
  saveProjectInStore' cfg env

-- save project in store
saveProjectInStore :: MimsaConfig -> Project ann -> PersistApp ProjectHash
saveProjectInStore cfg p = saveProjectInStore' cfg (p $> ())

saveProjectInStore' :: MimsaConfig -> Project () -> PersistApp ProjectHash
saveProjectInStore' cfg env = do
  let (jsonStr, hash) = contentAndHash (projectToSaved env)
  path <- liftIO $ getProjectPath cfg hash
  liftIO $ LBS.writeFile path jsonStr
  pure hash

--

storeItems :: Store a -> Set ExprHash
storeItems (Store s) = S.fromList (M.keys s)

loadBoundExpressions ::
  MimsaConfig ->
  Set ExprHash ->
  PersistApp (Store ())
loadBoundExpressions cfg hashes = do
  items' <-
    traverse
      ( \hash -> do
          item <- findExpr cfg hash
          pure (hash, item)
      )
      (S.toList hashes)
  pure
    (Store (M.fromList items'))

recursiveLoadBoundExpressions ::
  MimsaConfig ->
  Store () ->
  Set ExprHash ->
  PersistApp (Store ())
recursiveLoadBoundExpressions cfg existingStore hashes = do
  newStore <- loadBoundExpressions cfg (S.difference hashes (storeItems existingStore))
  let newHashes =
        S.difference
          ( S.unions $
              getDependencyHashes <$> M.elems (getStore newStore)
          )
          hashes
  if S.null newHashes
    then pure (existingStore <> newStore)
    else do
      moreStore <- recursiveLoadBoundExpressions cfg (existingStore <> newStore) newHashes
      pure (existingStore <> newStore <> moreStore)

--
