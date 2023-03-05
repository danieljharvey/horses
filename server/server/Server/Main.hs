{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Main
  ( server,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Monad (void)
import Control.Monad.Except hiding (mapError)
import Control.Monad.Reader
import qualified Data.Text.IO as T
import Language.Mimsa.Core
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Store
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Prometheus
import Prometheus
import Prometheus.Metric.GHC
import Servant
import Server.EnvVars (getMimsaEnv)
import Server.Servant
import Server.ServerConfig
import Server.ServerM
import Server.Types

createMetricsMiddleware :: Maybe Int -> Middleware
createMetricsMiddleware Nothing = id
createMetricsMiddleware (Just _metricsPort) = do
  prometheus def {prometheusInstrumentPrometheus = False}

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

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
mimsaApp :: MimsaEnvironment -> Middleware -> Application
mimsaApp mimsaEnv metricsMiddleware =
  metricsMiddleware $ corsMiddleware $ serve mimsaAPI (mimsaServer mimsaEnv)

createMimsaEnvironment :: ServerM (Error Annotation) MimsaEnvironment
createMimsaEnvironment = do
  cfg <- ask
  let project = stdlib
  _ <- mapError StoreErr (saveAllInStore (scRootPath cfg) (prjStore project))
  _ <- mapError StoreErr (saveModulesInStore (scRootPath cfg) (prjModuleStore project))
  ms <- liftIO (STM.newTVarIO (prjStore project))
  mms <- liftIO (STM.newTVarIO (prjModuleStore project))
  pure (MimsaEnvironment ms mms cfg)

server :: IO ()
server = do
  _ <- register ghcMetrics
  mimsaConfig' <- runExceptT getMimsaEnv
  case mimsaConfig' of
    Left e -> void (putStrLn e)
    Right cfg -> do
      rtn <- runServerM cfg serverM
      case rtn of
        -- error in initialisation
        Left e -> T.putStrLn (prettyPrint e)
        _ -> pure ()

serverM :: ServerM (Error Annotation) ()
serverM = do
  cfg <- ask
  mimsaEnv <- createMimsaEnvironment
  let port' = scPort cfg
  replOutput $ "Starting server on port " <> prettyPrint port' <> "..."
  let metricsMiddleware = createMetricsMiddleware (scPrometheusPort cfg)
  liftIO $ run port' (mimsaApp mimsaEnv metricsMiddleware) -- TODO - hoist Servant to use ServerM?
