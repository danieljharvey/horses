{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Store.Persistence
  ( fetchProjectItems,
    recursiveLoadModules,
  )
where

-- functions for Projects as opposed to the larger Store

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Core
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store.Storage
import Language.Mimsa.Types.Error.StoreError
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
  moduleStore <-
    recursiveLoadModules
      rootPath
      existingModuleStore
      (getItemsForAllVersions . projectModules $ sp)
  pure $
    projectFromSaved
      moduleStore
      existingStore
      sp

--

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

--
