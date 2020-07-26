{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store.Storage
  ( saveExpr,
    findExpr,
    getStoreExpressionHash,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import qualified Data.Hashable as Hash
import Data.Text (Text)
import qualified Data.Text.IO as T
import Language.Mimsa.Types
  ( ExprHash (..),
    Printer (..),
    StoreExpression (..),
  )
import System.Directory

-- get store folder, creating if it does not exist
-- the store folder usually lives in ~/.local/share
-- see https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html#t:XdgDirectory
getStoreFolder :: IO FilePath
getStoreFolder = do
  path <- getXdgDirectory XdgData "mimsa"
  createDirectoryIfMissing True path
  pure (path <> "/")

filePath :: FilePath -> ExprHash -> String
filePath storePath (ExprHash hash) = storePath <> show hash <> ".json"

getHash :: BS.ByteString -> ExprHash
getHash = ExprHash . Hash.hash

-- the store is where we save all the fucking bullshit

getStoreExpressionHash :: StoreExpression -> ExprHash
getStoreExpressionHash = getHash . JSON.encode

-- take an expression, save it, return ExprHash
saveExpr :: StoreExpression -> IO ExprHash
saveExpr expr = do
  storePath <- getStoreFolder
  let json = JSON.encode expr
  let exprHash = getHash json
  BS.writeFile (filePath storePath exprHash) json
  pure exprHash

-- find in the store
findExpr :: ExprHash -> ExceptT Text IO StoreExpression
findExpr hash = do
  storePath <- liftIO $ getStoreFolder
  json <- liftIO $ BS.readFile (filePath storePath hash)
  case JSON.decode json of
    Just a -> do
      liftIO $ T.putStrLn $ "Found expression for " <> prettyPrint hash
      pure a
    _ -> do
      liftIO $ T.putStrLn $ "Could not find expression for " <> prettyPrint hash
      error "Could not find!"
