{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store.Storage
  ( saveExpr,
    findExpr,
    getStoreExpressionHash,
    getStoreFolder,
    trySymlink,
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
import Language.Mimsa.Store.Hashing
import Language.Mimsa.Types.Error.StoreError
import Language.Mimsa.Types.Project.ProjectHash
import Language.Mimsa.Types.Store
import System.Directory

type StoreM = ExceptT StoreError IO

-- get store folder, creating if it does not exist
-- the store folder usually lives in ~/.local/share
-- see https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html#t:XdgDirectory
getStoreFolder :: String -> IO FilePath
getStoreFolder subFolder = do
  path <- getXdgDirectory XdgData ("mimsa/" <> subFolder)
  createDirectoryIfMissing True path
  pure (path <> "/")

-- when transpiling we write to the store then symlink it in place
-- this tries that, and failing that, copies the file (for Windows etc)
trySymlink :: String -> String -> StoreM ()
trySymlink from to = do
  symLinkCreated <- liftIO $ try (createFileLink from to)
  case (symLinkCreated :: Either IOError ()) of
    Right _ -> pure ()
    Left _ -> liftIO $ copyFile from to

getExpressionFolder :: IO FilePath
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

saveExpr :: StoreExpression ann -> StoreM ExprHash
saveExpr se = saveExpr' (se $> ())

-- take an expression, save it, return ExprHash
saveExpr' :: StoreExpression () -> StoreM ExprHash
saveExpr' expr = do
  storePath <- liftIO getExpressionFolder
  let (json, exprHash) = coerce $ contentAndHash expr
  liftIO $ T.putStrLn $ "Saved expression for " <> prettyPrint exprHash
  liftIO $ BS.writeFile (filePath storePath exprHash) json
  pure exprHash

findExpr ::
  ExprHash ->
  StoreM (StoreExpression ())
findExpr = findExprInLocalStore

-- find in the store
findExprInLocalStore :: ExprHash -> StoreM (StoreExpression ())
findExprInLocalStore hash = do
  storePath <- liftIO getExpressionFolder
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
