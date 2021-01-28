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
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import Language.Mimsa.Server.EnvVars
import Language.Mimsa.Store.Hashing
import Language.Mimsa.Types.Error.StoreError
import Language.Mimsa.Types.Project.ProjectHash
import Language.Mimsa.Types.Store
import System.Directory

type StoreM = ExceptT StoreError IO

-- get store folder, creating if it does not exist
-- the store folder usually lives in ~/.local/share
-- see https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html#t:XdgDirectory
getStoreFolder :: MimsaConfig -> String -> IO FilePath
getStoreFolder cfg subFolder = do
  let path = storeRootPath cfg <> "/" <> subFolder
  createDirectoryIfMissing True path
  pure (path <> "/")

-- try copying a file from a to b
tryCopy :: String -> String -> StoreM ()
tryCopy from to = do
  fileCopied <- liftIO $ try (copyFile from to)
  case (fileCopied :: Either IOError ()) of
    Right _ -> do
      liftIO $ putStrLn $ "File copied from " <> from <> " to " <> to
    Left _ -> pure ()

getExpressionFolder :: MimsaConfig -> IO FilePath
getExpressionFolder cfg = getStoreFolder cfg "expressions"

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

saveExpr :: MimsaConfig -> StoreExpression ann -> StoreM ExprHash
saveExpr cfg se = saveExpr' cfg (se $> ())

-- take an expression, save it, return ExprHash
saveExpr' :: MimsaConfig -> StoreExpression () -> StoreM ExprHash
saveExpr' cfg expr = do
  storePath <- liftIO $ getExpressionFolder cfg
  let path = filePath storePath exprHash
      (json, exprHash) = coerce $ contentAndHash expr
  exists <- liftIO $ doesFileExist path
  if exists
    then liftIO $ T.putStrLn $ "Expression for " <> prettyPrint exprHash <> " already exists"
    else do
      liftIO $ T.putStrLn $ "Saved expression for " <> prettyPrint exprHash
      liftIO $ BS.writeFile (filePath storePath exprHash) json
  pure exprHash

findExpr ::
  MimsaConfig ->
  ExprHash ->
  StoreM (StoreExpression ())
findExpr = findExprInLocalStore

-- find in the store
findExprInLocalStore :: MimsaConfig -> ExprHash -> StoreM (StoreExpression ())
findExprInLocalStore cfg hash = do
  storePath <- liftIO (getExpressionFolder cfg)
  json <- liftIO $ try $ BS.readFile (filePath storePath hash)
  case (json :: Either IOError BS.ByteString) of
    Left _ -> throwError $ CouldNotReadFilePath (filePath storePath hash)
    Right json' ->
      case JSON.decode json' of
        Just storeExpr ->
          case validateStoreExpression storeExpr hash of
            Right se -> do
              liftIO $ T.putStrLn $ "Found expression for " <> prettyPrint hash
              pure se
            Left e -> throwError e
        _ ->
          throwError (CouldNotDecodeJson hash)
