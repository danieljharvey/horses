module Language.Mimsa.Store (saveExpr, findExpr, loadEnvironment, saveEnvironment) where

import Control.Exception (try)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import qualified Data.Hashable as Hash
import qualified Data.Map as M
import Language.Mimsa.Types (Expr (..), ExprHash (..), StoreEnv (..))

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
findExpr :: ExprHash -> IO (Either String Expr)
findExpr hash = do
  json <- BS.readFile (filePath hash)
  case JSON.decode json of
    Just a -> pure (Right a)
    _ -> pure (Left "Could not find!")

getHash :: BS.ByteString -> ExprHash
getHash = ExprHash . Hash.hash

findExpr10x :: ExprHash -> IO Expr
findExpr10x hash = do
  hash' <- findExpr hash
  case hash' of
    Right a -> pure a
    _ -> error "yolo"

hush :: Either IOError a -> Maybe a
hush (Right a) = Just a
hush _ = Nothing

-- load environment.json and any hashed exprs mentioned in it
-- should probably consider loading the exprs lazily as required in future
loadEnvironment :: IO (Maybe StoreEnv)
loadEnvironment = do
  envJson <- try $ BS.readFile envPath
  case hush envJson >>= JSON.decode of
    Just bindings' -> do
      items' <-
        traverse
          ( \(_, hash) -> do
              item <- findExpr10x hash
              pure (hash, item)
          )
          (M.toList bindings')
      pure $ Just (StoreEnv (M.fromList items') bindings')
    _ -> pure Nothing

--
saveEnvironment :: StoreEnv -> IO ()
saveEnvironment env = do
  let jsonStr = JSON.encode (bindings env)
  BS.writeFile envPath jsonStr
