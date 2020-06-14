{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store
  ( saveExpr,
    findExpr,
    loadEnvironment,
    loadBoundExpressions,
    saveEnvironment,
    module Language.Mimsa.Store.Resolver,
  )
where

import Control.Exception (try)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import qualified Data.Hashable as Hash
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Store.Resolver
import Language.Mimsa.Types
  ( Bindings (..),
    Expr (..),
    ExprHash (..),
    Store (..),
    StoreEnv (..),
  )

storePath :: String
storePath = "./store/"

filePath :: ExprHash -> String
filePath (ExprHash hash) = storePath <> show hash <> ".json"

envPath :: String
envPath = storePath <> "environment.json"

-- the store is where we save all the fucking bullshit

-- take an expression, save it, return ExprHash
saveExpr :: Expr -> IO ExprHash
saveExpr expr = do
  let json = JSON.encode expr
  let exprHash = getHash json
  BS.writeFile (filePath exprHash) json
  pure exprHash

-- find in the store
findExpr :: ExprHash -> IO (Either Text Expr)
findExpr hash = do
  json <- BS.readFile (filePath hash)
  case JSON.decode json of
    Just a -> pure (Right a)
    _ -> pure (Left "Could not find!")

getHash :: BS.ByteString -> ExprHash
getHash = ExprHash . Hash.hash

hush :: Either IOError a -> Maybe a
hush (Right a) = Just a
hush _ = Nothing

-- load environment.json and any hashed exprs mentioned in it
-- should probably consider loading the exprs lazily as required in future
loadEnvironment :: IO (Maybe StoreEnv)
loadEnvironment = do
  envJson <- try $ BS.readFile envPath
  case hush envJson >>= JSON.decode of
    Just b@(Bindings bindings') -> do
      items' <- loadBoundExpressions (S.fromList (M.elems bindings'))
      case items' of
        Right store' -> pure $ Just (StoreEnv store' b)
        _ -> pure Nothing
    _ -> pure Nothing

--

loadBoundExpressions :: Set ExprHash -> IO (Either Text Store)
loadBoundExpressions hashes = do
  items' <-
    traverse
      ( \hash -> do
          item <- findExpr hash
          pure $ (,) <$> pure hash <*> item
      )
      (S.toList hashes)
  case sequence items' of
    Right goodItems -> pure (Right (Store (M.fromList goodItems)))
    Left a -> pure (Left a)

--
saveEnvironment :: StoreEnv -> IO ()
saveEnvironment env = do
  let jsonStr = JSON.encode (bindings env)
  BS.writeFile envPath jsonStr
