{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Router.Server (runServer) where

import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import GHC.Generics
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Req
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Router.Config
import Router.Environment
import Router.Fetch
import Router.Proxy
import Router.Unzip
import Servant

type HealthzAPI =
  "healthz" :> Get '[JSON] String

healthzHandler :: Server HealthzAPI
healthzHandler = pure "OK"

type FetchExprAPI =
  "fetch" :> "expr"
    :> ReqBody '[JSON] FetchExprRequest
    :> Post '[JSON] Key

newtype FetchExprRequest = FetchExprRequest
  { feExprHash :: ExprHash
  }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON)

fetchExprHandler :: Config -> Environment -> Server FetchExprAPI
fetchExprHandler cfg env (FetchExprRequest exprHash) = do
  let compilePayload =
        JSON.object
          [ "chExprHash" .= exprHash,
            "chRuntime" .= ("task-server" :: String)
          ]
  eitherBS <-
    ( if cfgUseHttps cfg
        then fetchIO (https (T.pack $ cfgMimsaBaseUrl cfg) /: "compile" /: "hash") (cfgMimsaPort cfg) compilePayload
        else fetchIO (http (T.pack $ cfgMimsaBaseUrl cfg) /: "compile" /: "hash") (cfgMimsaPort cfg) compilePayload
      )
  case eitherBS of
    Right bs -> do
      _ <- unzipFiles cfg bs
      addRoute env exprHash
    Left (FourXX msg) -> throwError (err400 {errBody = msg})
    Left _ -> throwError err500

type Router =
  HealthzAPI
    :<|> FetchExprAPI
    :<|> Raw

router :: HTTP.Manager -> Config -> Environment -> Server Router
router manager cfg env =
  healthzHandler
    :<|> fetchExprHandler cfg env
    :<|> Tagged (proxyToInterpreter env manager)

server :: Proxy Router
server = Proxy

-- allow GET and POST with JSON
corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just policy)
  where
    sc = simpleCorsResourcePolicy
    policy =
      sc
        { corsMethods =
            corsMethods sc <> [methodGet, methodPost, methodOptions],
          corsRequestHeaders =
            corsRequestHeaders sc <> [hContentType]
        }

runServer :: Config -> IO ()
runServer cfg = do
  env <- newEnvironment
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  putStrLn $ "Router running at port " <> show (cfgPort cfg)
  runSettings appSettings $ corsMiddleware $ serve server (router manager cfg env)
  where
    appSettings = setPort (cfgPort cfg) $ setHost "0.0.0.0" defaultSettings
