{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store.Storage
  ( saveExpr,
    findExpr,
    getStoreExpressionHash,
    getStoreFolder,
    tryCopy,
    storeSize,
    saveFile,
    saveAllInStore,
    serialiseStoreExpression,
    deserialiseStoreExpression,
  )
where

import Control.Exception
import Control.Monad.Except
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import Data.Coerce
import Data.Foldable
import Data.Functor
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Mimsa.Actions.Types as Actions
import Language.Mimsa.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Store.Hashing
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.MimsaConfig
import Language.Mimsa.Types.NullUnit
import Language.Mimsa.Types.Project.ProjectHash
import Language.Mimsa.Types.Store
import System.Directory

-- get store folder, creating if it does not exist
-- the store folder usually lives in ~/.local/share
-- see https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html#t:XdgDirectory
getStoreFolder :: String -> MimsaM e FilePath
getStoreFolder subFolder = do
  cfg <- getMimsaConfig
  let path = storeRootPath cfg <> "/" <> subFolder
  liftIO $ createDirectoryIfMissing True path
  pure (path <> "/")

-- try copying a file from a to b
tryCopy :: String -> String -> MimsaM StoreError ()
tryCopy from to = do
  fileCopied <- liftIO $ try (copyFile from to)
  case (fileCopied :: Either IOError ()) of
    Right _ -> do
      logDebug $ T.pack $ "File copied from " <> from <> " to " <> to
    Left _ -> pure ()

getExpressionFolder :: MimsaM e FilePath
getExpressionFolder = getStoreFolder "expressions"

filePath :: FilePath -> ExprHash -> String
filePath storePath hash = storePath <> show hash <> ".json"

storeSize :: Store a -> Int
storeSize (Store s) = M.size s

-- the store is where we save all the fucking bullshit

saveAllInStore :: Store ann -> MimsaM StoreError ()
saveAllInStore store = do
  traverse_ saveExpr (getStore store)

getStoreExpressionHash :: StoreExpression ann -> ExprHash
getStoreExpressionHash = snd . serialiseStoreExpression

-- | check a loaded store expression matches its hash
validateStoreExpression ::
  StoreExpression NullUnit ->
  ExprHash ->
  Either StoreError (StoreExpression NullUnit)
validateStoreExpression storeExpr exprHash =
  if getStoreExpressionHash storeExpr == exprHash
    then pure storeExpr
    else
      Left $
        ExpressionDoesNotMatchHash
          exprHash
          (getStoreExpressionHash storeExpr)

saveExpr :: StoreExpression ann -> MimsaM StoreError ExprHash
saveExpr se =
  saveExpr' (se $> NullUnit)

-- take an expression, save it, return ExprHash
saveExpr' :: StoreExpression NullUnit -> MimsaM StoreError ExprHash
saveExpr' storeExpr = do
  storePath <- getExpressionFolder
  let path = filePath storePath exprHash
      (json, exprHash) = serialiseStoreExpression storeExpr
  exists <- liftIO $ doesFileExist path
  if exists
    then logDebug $ "Expression for " <> prettyPrint exprHash <> " already exists"
    else do
      logDebug $ "Saved expression for " <> prettyPrint exprHash
      liftIO $ BS.writeFile (filePath storePath exprHash) json
  pure exprHash

findExpr ::
  ExprHash ->
  MimsaM StoreError (StoreExpression ())
findExpr = fmap ($> ()) . findExprInLocalStore

-- this is the only encode we should be doing
serialiseStoreExpression :: StoreExpression ann -> (BS.ByteString, ExprHash)
serialiseStoreExpression se = coerce $ contentAndHash (se $> NullUnit)

-- this is the only json decode we should be doing
deserialiseStoreExpression :: BS.ByteString -> Maybe (StoreExpression NullUnit)
deserialiseStoreExpression =
  JSON.decode

-- find in the store
findExprInLocalStore :: ExprHash -> MimsaM StoreError (StoreExpression NullUnit)
findExprInLocalStore hash = do
  storePath <- getExpressionFolder
  json <- liftIO $ try $ BS.readFile (filePath storePath hash)
  case (json :: Either IOError BS.ByteString) of
    Left _ -> throwError $ CouldNotReadFilePath StoreExprFile (filePath storePath hash)
    Right json' -> do
      case deserialiseStoreExpression json' of
        Nothing -> throwError CouldNotDecodeByteString
        Just storeExpr ->
          case validateStoreExpression storeExpr hash of
            Right se -> do
              logDebug $ "Found expression for " <> prettyPrint hash
              pure se
            Left e -> throwError e

-- | given an expression to save, save it
-- | some sort of catch / error?
saveFile ::
  (Actions.SavePath, Actions.SaveFilename, Actions.SaveContents) ->
  MimsaM StoreError ()
saveFile (path, filename, content) = do
  fullPath <- getStoreFolder (show path)
  let savePath = fullPath <> show filename
  logDebug $ "Saving to " <> T.pack savePath
  liftIO $ T.writeFile savePath (coerce content)
