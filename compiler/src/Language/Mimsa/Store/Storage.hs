{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store.Storage
  ( saveExpr,
    findExpr,
    findModule,
    getStoreExpressionHash,
    getStoreFolder,
    tryCopy,
    storeSize,
    saveFile,
    saveAllInStore,
    saveModulesInStore,
    serialiseStoreExpression,
    deserialiseStoreExpression,
    getModuleHash,
  )
where

import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import Data.Coerce
import Data.Foldable
import Data.Functor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Mimsa.Actions.Types as Actions
import Language.Mimsa.Core
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Store.Hashing
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project.ProjectHash
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Store.RootPath
import System.Directory

-- create subfolder in store, creating if necessary
getStoreFolder :: (MonadIO m) => RootPath -> String -> m FilePath
getStoreFolder (RootPath rootPath) subFolder = do
  let path = rootPath <> "/" <> subFolder
  liftIO $ createDirectoryIfMissing True path
  pure (path <> "/")

-- try copying a file from a to b
tryCopy ::
  (MonadIO m, MonadLogger m) =>
  String ->
  String ->
  m ()
tryCopy from to = do
  fileCopied <- liftIO $ try (copyFile from to)
  case (fileCopied :: Either IOError ()) of
    Right _ -> do
      logDebugN $ T.pack $ "File copied from " <> from <> " to " <> to
    Left _ -> pure ()

getExpressionFolder :: (MonadIO m) => RootPath -> m FilePath
getExpressionFolder rootPath = getStoreFolder rootPath "expressions"

getModuleFolder :: (MonadIO m) => RootPath -> m FilePath
getModuleFolder rootPath = getStoreFolder rootPath "modules"

filePath :: FilePath -> ExprHash -> String
filePath storePath hash = storePath <> show hash <> ".json"

storeSize :: Store a -> Int
storeSize (Store s) = M.size s

-- the store is where we save all the fucking bullshit

saveAllInStore ::
  (MonadIO m, MonadLogger m) =>
  RootPath ->
  Store ann ->
  m ()
saveAllInStore rootPath store = do
  traverse_ (saveExpr rootPath) (getStore store)

saveModulesInStore ::
  (MonadIO m, MonadLogger m) =>
  RootPath ->
  Map ModuleHash (Module ann) ->
  m ()
saveModulesInStore rootPath =
  traverse_ (saveModule rootPath)

getModuleHash :: Module ann -> ModuleHash
getModuleHash = snd . serializeModule

-- | check a loaded store expression matches its hash
validateModule ::
  Module () ->
  ModuleHash ->
  Either StoreError (Module ())
validateModule mod' moduleHash =
  if getModuleHash mod' == moduleHash
    then pure mod'
    else
      Left $
        ExpressionDoesNotMatchHash
          (coerce moduleHash)
          (coerce $ getModuleHash mod')

getStoreExpressionHash :: StoreExpression ann -> ExprHash
getStoreExpressionHash = snd . serialiseStoreExpression

-- | check a loaded store expression matches its hash
validateStoreExpression ::
  StoreExpression () ->
  ExprHash ->
  Either StoreError (StoreExpression ())
validateStoreExpression storeExpr exprHash =
  if getStoreExpressionHash storeExpr == exprHash
    then pure storeExpr
    else
      Left $
        ExpressionDoesNotMatchHash
          exprHash
          (getStoreExpressionHash storeExpr)

saveExpr ::
  (MonadIO m, MonadLogger m) =>
  RootPath ->
  StoreExpression ann ->
  m ExprHash
saveExpr rootPath se =
  saveExpr' rootPath (se $> ())

-- take an expression, save it, return ExprHash
saveExpr' ::
  (MonadIO m, MonadLogger m) =>
  RootPath ->
  StoreExpression () ->
  m ExprHash
saveExpr' rootPath storeExpr = do
  storePath <- getExpressionFolder rootPath
  let path = filePath storePath exprHash
      (json, exprHash) = serialiseStoreExpression storeExpr
  exists <- liftIO $ doesFileExist path
  if exists
    then logDebugN $ "Expression for " <> prettyPrint exprHash <> " already exists"
    else do
      logDebugN $ "Saved expression for " <> prettyPrint exprHash
      liftIO $ BS.writeFile (filePath storePath exprHash) json
  pure exprHash

---- save modules

saveModule ::
  (MonadIO m, MonadLogger m) =>
  RootPath ->
  Module ann ->
  m ModuleHash
saveModule rootPath mod' =
  saveModule' rootPath (mod' $> ())

-- take an expression, save it, return ExprHash
saveModule' ::
  (MonadIO m, MonadLogger m) =>
  RootPath ->
  Module () ->
  m ModuleHash
saveModule' rootPath mod' = do
  storePath <- getModuleFolder rootPath
  let (json, moduleHash) = serializeModule mod'
      path = filePath storePath (coerce moduleHash)
  exists <- liftIO $ doesFileExist path
  if exists
    then logDebugN $ "Module for " <> prettyPrint moduleHash <> " already exists"
    else do
      logDebugN $ "Saved module for " <> prettyPrint moduleHash
      liftIO $ BS.writeFile (filePath storePath (coerce moduleHash)) json
  pure moduleHash

-- this is the only encode we should be doing
serialiseStoreExpression :: StoreExpression ann -> (BS.ByteString, ExprHash)
serialiseStoreExpression se = coerce $ contentAndHash (se $> ())

-- this is the only json decode we should be doing
deserialiseStoreExpression :: BS.ByteString -> Maybe (StoreExpression ())
deserialiseStoreExpression =
  JSON.decode

findExpr ::
  (MonadIO m, MonadError StoreError m, MonadLogger m) =>
  RootPath ->
  ExprHash ->
  m (StoreExpression ())
findExpr rootPath = fmap ($> ()) . findExprInLocalStore rootPath

-- find in the store
findExprInLocalStore ::
  (MonadIO m, MonadError StoreError m, MonadLogger m) =>
  RootPath ->
  ExprHash ->
  m (StoreExpression ())
findExprInLocalStore rootPath hash = do
  storePath <- getExpressionFolder rootPath
  json <- liftIO $ try $ BS.readFile (filePath storePath hash)
  case (json :: Either IOError BS.ByteString) of
    Left _ -> throwError $ CouldNotReadFilePath StoreExprFile (filePath storePath hash)
    Right json' -> do
      case deserialiseStoreExpression json' of
        Nothing -> throwError CouldNotDecodeByteString
        Just storeExpr ->
          case validateStoreExpression storeExpr hash of
            Right se -> do
              logDebugN $ "Found expression for " <> prettyPrint hash
              pure se
            Left e -> throwError e

findModule ::
  (MonadIO m, MonadError StoreError m, MonadLogger m) =>
  RootPath ->
  ModuleHash ->
  m (Module ())
findModule rootPath = fmap ($> ()) . findModuleInLocalStore rootPath

-- find in the store
findModuleInLocalStore ::
  (MonadIO m, MonadError StoreError m, MonadLogger m) =>
  RootPath ->
  ModuleHash ->
  m (Module ())
findModuleInLocalStore rootPath hash = do
  storePath <- getModuleFolder rootPath
  json <- liftIO $ try $ BS.readFile (filePath storePath (coerce hash))
  case (json :: Either IOError BS.ByteString) of
    Left _ -> throwError $ CouldNotReadFilePath ModuleFile (filePath storePath (coerce hash))
    Right json' -> do
      case deserializeModule json' of
        Nothing -> throwError CouldNotDecodeByteString
        Just mod' ->
          case validateModule mod' hash of
            Right validatedModule -> do
              logDebugN $ "Found module for " <> prettyPrint hash
              pure validatedModule
            Left e -> throwError e

-- | given an expression to save, save it
-- | some sort of catch / error?
saveFile ::
  (MonadIO m, MonadLogger m) =>
  RootPath ->
  (Actions.SavePath, Actions.SaveFilename, Actions.SaveContents) ->
  m ()
saveFile rootPath (path, filename, content) = do
  fullPath <- getStoreFolder rootPath (show path)
  let savePath = fullPath <> show filename
  logDebugN $ "Saving to " <> T.pack savePath
  liftIO $ T.writeFile savePath (coerce content)
