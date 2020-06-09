module Store (saveExpr, findExpr) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import qualified Data.Hashable as Hash
import Types (Expr (..), ExprHash (..))

storePath :: String
storePath = "./store/"

filePath :: ExprHash -> String
filePath (ExprHash hash) = storePath <> show hash <> ".json"

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
