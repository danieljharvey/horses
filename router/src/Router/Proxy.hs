{-# LANGUAGE OverloadedStrings #-}

module Router.Proxy (proxyToInterpreter) where

import Data.Coerce
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.ReverseProxy
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Rewrite as Wai
import Router.Environment

-- in which our hero forwards all the other requests to the interpreters

-- exprHash = ExprHash "68b7f2c95426655cb1467c1234fb9de23f995cc186647ced6b66426d1acd451a"

proxyToInterpreter :: Environment -> HTTP.Manager -> Wai.Application
proxyToInterpreter env = waiProxyTo handler exceptionHandler
  where
    handler = \req -> do
      withHeader <- case getFirstPart req >>= readKey of
        Nothing -> pure req
        Just key -> do
          exprHash <- lookupRoute env key
          case exprHash of
            Nothing -> pure req
            Just exprHash' ->
              pure $ addExprHashHeader exprHash' req
      print (Wai.requestHeaders withHeader)
      pure
        ( WPRModifiedRequest
            (removeFirstPart withHeader)
            (ProxyDest "runner" 8777)
        )
    exceptionHandler = defaultOnExc

getFirstPart :: Wai.Request -> Maybe Text
getFirstPart req = listToMaybe (Wai.pathInfo req)

readKey :: Text -> Maybe Key
readKey t = case T.decimal t of
  Right (a, _) -> Just (Key a)
  _ -> Nothing

removeFirstPart :: Wai.Request -> Wai.Request
removeFirstPart =
  Wai.rewriteRequestPure
    (\(parts, query) -> const (drop 1 parts, query))

addExprHashHeader :: ExprHash -> Wai.Request -> Wai.Request
addExprHashHeader exprHash req =
  req
    { Wai.requestHeaders =
        Wai.requestHeaders req <> [("mimsa-root-exprhash", T.encodeUtf8 (coerce exprHash))]
    }
