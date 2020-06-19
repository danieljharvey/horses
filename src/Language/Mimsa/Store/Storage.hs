{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store.Storage
  ( saveExpr,
    findExpr,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import qualified Data.Hashable as Hash
import Data.Text (Text)
import qualified Data.Text.IO as T
import Language.Mimsa.Syntax
import Language.Mimsa.Types
  ( ExprHash (..),
    StoreExpression (..),
  )

storePath :: String
storePath = "./store/"

filePath :: ExprHash -> String
filePath (ExprHash hash) = storePath <> show hash <> ".json"

getHash :: BS.ByteString -> ExprHash
getHash = ExprHash . Hash.hash

-- the store is where we save all the fucking bullshit

-- take an expression, save it, return ExprHash
saveExpr :: StoreExpression -> IO ExprHash
saveExpr expr = do
  let json = JSON.encode expr
  let exprHash = getHash json
  BS.writeFile (filePath exprHash) json
  pure exprHash

-- find in the store
findExpr :: ExprHash -> ExceptT Text IO StoreExpression
findExpr hash = do
  json <- liftIO $ BS.readFile (filePath hash)
  case JSON.decode json of
    Just a -> do
      liftIO $ T.putStrLn $ "Found expression for " <> prettyPrint hash
      pure a
    _ -> do
      liftIO $ T.putStrLn $ "Could not find expression for " <> prettyPrint hash
      error "Could not find!"
