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
import Network.Wai.Handler.Warp
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
  bs <-
    if cfgUseHttps cfg
      then fetch (https (T.pack $ cfgMimsaBaseUrl cfg) /: "compile" /: "hash") (cfgMimsaPort cfg) compilePayload
      else fetch (http (T.pack $ cfgMimsaBaseUrl cfg) /: "compile" /: "hash") (cfgMimsaPort cfg) compilePayload
  _ <- unzipFiles cfg bs
  addRoute env exprHash

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

runServer :: Config -> IO ()
runServer cfg = do
  env <- newEnvironment
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  putStrLn $ "Router running at port " <> show (cfgPort cfg)
  runSettings appSettings $ serve server (router manager cfg env)
  where
    appSettings = setPort (cfgPort cfg) $ setHost "0.0.0.0" defaultSettings
