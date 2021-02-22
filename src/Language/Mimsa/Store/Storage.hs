{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store.Storage
  ( saveExpr,
    findExpr,
    getStoreExpressionHash,
    getStoreFolder,
    tryCopy,
    storeSize,
  )
where

import Control.Exception
import Control.Monad.Except
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import Data.Coerce
import Data.Functor
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Store.Hashing
import Language.Mimsa.Types.Error.StoreError
import Language.Mimsa.Types.MimsaConfig
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

getStoreExpressionHash :: StoreExpression ann -> ExprHash
getStoreExpressionHash se = getStoreExpressionHash' (se $> ())

getStoreExpressionHash' :: StoreExpression () -> ExprHash
getStoreExpressionHash' = coerce . snd . contentAndHash

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

saveExpr :: StoreExpression ann -> MimsaM StoreError ExprHash
saveExpr se =
  saveExpr' (se $> ())

-- take an expression, save it, return ExprHash
saveExpr' :: StoreExpression () -> MimsaM StoreError ExprHash
saveExpr' expr = do
  storePath <- getExpressionFolder
  let path = filePath storePath exprHash
      (json, exprHash) = coerce $ contentAndHash expr
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
findExpr = findExprInLocalStore

-- find in the store
findExprInLocalStore :: ExprHash -> MimsaM StoreError (StoreExpression ())
findExprInLocalStore hash = do
  storePath <- getExpressionFolder
  json <- liftIO $ try $ BS.readFile (filePath storePath hash)
  case (json :: Either IOError BS.ByteString) of
    Left _ -> throwError $ CouldNotReadFilePath (filePath storePath hash)
    Right json' ->
      case JSON.decode json' of
        Just storeExpr ->
          case validateStoreExpression storeExpr hash of
            Right se -> do
              logDebug $ "Found expression for " <> prettyPrint hash
              pure se
            Left e -> throwError e
        _ ->
          throwError (CouldNotDecodeJson hash)
