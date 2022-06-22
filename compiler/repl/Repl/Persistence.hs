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
import qualified Data.Text as T
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store.Hashing
import Language.Mimsa.Store.Persistence
import Language.Mimsa.Types.Error.StoreError
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store.RootPath
import Repl.Types

projectFilePath :: RootPath -> String
projectFilePath (RootPath rp) = rp <> "/mimsa.json"

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
  logDebugN ("Attempting to load project file at " <> T.pack (projectFilePath rootPath))
  project' <- liftIO $ try $ LBS.readFile (projectFilePath rootPath)
  case project' of
    Left (_ :: IOError) -> throwError (CouldNotReadFilePath ProjectFile (projectFilePath rootPath))
    Right json' -> do
      logDebugN "Project file found"
      case JSON.decode json' of
        Just sp -> do
          logDebugN "Project file successfully decoded. Fetching project items...."
          fetchProjectItems rootPath mempty sp -- we're starting from scratch with this one
        _ -> throwError $ CouldNotDecodeFile (projectFilePath rootPath)

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
  let (jsonStr, hash) = contentAndHash (projectToSaved env)
  success <- liftIO $ try $ LBS.writeFile (projectFilePath rootPath) jsonStr
  case success of
    Left (_ :: IOError) ->
      throwError (CouldNotWriteFilePath ProjectFile (projectFilePath rootPath))
    Right _ -> do
      logDebugN ("Successfully updated project file at " <> T.pack (projectFilePath rootPath))
      pure hash

--
