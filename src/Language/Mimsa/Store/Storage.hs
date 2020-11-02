{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Store.Storage
  ( saveExpr,
    findExpr,
    getStoreExpressionHash,
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad.Except
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import Data.Functor
import qualified Data.Hashable as Hash
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Network.HTTP.Req
import System.Directory
import Text.URI

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

downloadPath :: ServerUrl -> ExprHash -> Text
downloadPath (ServerUrl url) (ExprHash hash) = url <> T.pack (show hash) <> ".json"

getHash :: BS.ByteString -> ExprHash
getHash = ExprHash . Hash.hash

-- the store is where we save all the fucking bullshit

getStoreExpressionHash :: StoreExpression ann -> ExprHash
getStoreExpressionHash se = getStoreExpressionHash' (se $> ())

getStoreExpressionHash' :: StoreExpression () -> ExprHash
getStoreExpressionHash' = getHash . JSON.encode

validateStoreExpression ::
  StoreExpression () ->
  ExprHash ->
  Either Text (StoreExpression ())
validateStoreExpression storeExpr exprHash =
  if getStoreExpressionHash storeExpr == exprHash
    then pure storeExpr
    else
      Left $
        "Expected hash of "
          <> prettyPrint exprHash
          <> " but instead received "
          <> prettyPrint (getStoreExpressionHash storeExpr)

saveExpr :: StoreExpression ann -> IO ExprHash
saveExpr se = saveExpr' (se $> ())

-- take an expression, save it, return ExprHash
saveExpr' :: StoreExpression () -> IO ExprHash
saveExpr' expr = do
  storePath <- getStoreFolder
  let json = JSON.encode expr
  let exprHash = getHash json
  BS.writeFile (filePath storePath exprHash) json
  pure exprHash

findExpr :: [ServerUrl] -> ExprHash -> ExceptT Text IO (StoreExpression ())
findExpr urls hash = findExprInLocalStore hash <|> tryServers hash urls

tryServers :: ExprHash -> [ServerUrl] -> ExceptT Text IO (StoreExpression ())
tryServers _ [] = throwError "Could not find expression on any server"
tryServers hash (url : urls) = findExprFromServer url hash <|> tryServers hash urls

getPath :: ExprHash -> ServerUrl -> ExceptT Text IO (Url Https)
getPath hash serverUrl' = do
  let path = downloadPath serverUrl' hash
  uri <- mkURI path
  case useHttpsURI uri of
    Just (url, _) -> pure url
    _ -> throwError "oh no"

-- find in the store
findExprFromServer :: ServerUrl -> ExprHash -> ExceptT Text IO (StoreExpression ())
findExprFromServer serverUrl' hash = do
  path <- getPath hash serverUrl'
  body <-
    runReq defaultHttpConfig $
      req
        GET -- method
        path -- safe by construction URL
        NoReqBody
        jsonResponse -- specify how to interpret response
        mempty -- query params, headers, explicit port number, etc.
  liftIO $ T.putStrLn $
    "Downloading expression for " <> prettyPrint hash
      <> " from "
      <> getServerUrl serverUrl'
  let storeExpr = responseBody body
  case validateStoreExpression storeExpr hash of
    Right storeExpr' -> do
      _ <- liftIO $ saveExpr storeExpr' -- keep it in our local store
      pure storeExpr'
    Left e ->
      throwError e

-- find in the store
findExprInLocalStore :: ExprHash -> ExceptT Text IO (StoreExpression ())
findExprInLocalStore hash = do
  storePath <- liftIO getStoreFolder
  json <- liftIO $ try $ BS.readFile (filePath storePath hash)
  case (json :: Either IOError BS.ByteString) of
    Left _ -> throwError $ "Could not read file " <> T.pack (filePath storePath hash)
    Right json' ->
      case JSON.decode json' of
        Just storeExpr ->
          case validateStoreExpression storeExpr hash of
            Right se -> do
              liftIO $ T.putStrLn $ "Found expression for " <> prettyPrint hash
              pure se
            Left e -> throwError e
        _ -> do
          liftIO $ T.putStrLn $ "Could not find expression for " <> prettyPrint hash
          throwError "Could not find!"
