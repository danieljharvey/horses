{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Store.Persistence
  ( fetchProjectItems,
    recursiveLoadBoundExpressions,
    recursiveLoadModules,
  )
where

-- functions for Projects as opposed to the larger Store

import Control.Monad.Except
import Control.Monad.Logger
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store.Storage
import Language.Mimsa.Types.Error.StoreError
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Store.RootPath

fetchProjectItems ::
  (MonadIO m, MonadError StoreError m, MonadLogger m) =>
  RootPath ->
  Store () ->
  Map ModuleHash (Module ()) ->
  SaveProject ->
  m (Project ())
fetchProjectItems rootPath existingStore existingModuleStore sp = do
  store' <-
    recursiveLoadBoundExpressions
      rootPath
      existingStore
      (getItemsForAllVersions . projectBindings $ sp)
  typeStore' <-
    recursiveLoadBoundExpressions
      rootPath
      existingStore
      (getItemsForAllVersions . projectTypes $ sp)
  testStore <-
    recursiveLoadBoundExpressions
      rootPath
      existingStore
      ( M.keysSet
          ( projectUnitTests sp
          )
          <> M.keysSet (projectPropertyTests sp)
      )
  moduleStore <-
    recursiveLoadModules
      rootPath
      existingModuleStore
      (getItemsForAllVersions . projectModules $ sp)
  pure $
    projectFromSaved
      moduleStore
      ( existingStore
          <> store'
          <> typeStore'
          <> testStore
      )
      sp

--

storeItems :: Store a -> Set ExprHash
storeItems (Store s) = S.fromList (M.keys s)

loadModules ::
  (MonadIO m, MonadError StoreError m, MonadLogger m) =>
  RootPath ->
  Set ModuleHash ->
  m (Map ModuleHash (Module ()))
loadModules rootPath hashes = do
  M.fromList
    <$> traverse
      ( \hash -> do
          item <- findModule rootPath hash
          pure (hash, item)
      )
      (S.toList hashes)

recursiveLoadModules ::
  (MonadIO m, MonadError StoreError m, MonadLogger m) =>
  RootPath ->
  Map ModuleHash (Module ()) ->
  Set ModuleHash ->
  m (Map ModuleHash (Module ()))
recursiveLoadModules rootPath existingStore hashes = do
  newStore <-
    loadModules
      rootPath
      (S.difference hashes (M.keysSet existingStore))
  let newHashes =
        S.difference
          ( S.unions $
              getModuleDependencyHashes <$> M.elems newStore
          )
          hashes
  if S.null newHashes
    then pure (existingStore <> newStore)
    else do
      moreStore <-
        recursiveLoadModules
          rootPath
          (existingStore <> newStore)
          newHashes
      pure (existingStore <> newStore <> moreStore)

loadBoundExpressions ::
  (MonadIO m, MonadError StoreError m, MonadLogger m) =>
  RootPath ->
  Set ExprHash ->
  m (Store ())
loadBoundExpressions rootPath hashes = do
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
  (MonadIO m, MonadError StoreError m, MonadLogger m) =>
  RootPath ->
  Store () ->
  Set ExprHash ->
  m (Store ())
recursiveLoadBoundExpressions rootPath existingStore hashes = do
  newStore <-
    loadBoundExpressions
      rootPath
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
          rootPath
          (existingStore <> newStore)
          newHashes
      pure (existingStore <> newStore <> moreStore)

--
