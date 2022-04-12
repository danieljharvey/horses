{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Main
  ( server,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
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
import Server.Persistence
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
  env <- getDefaultProject
  _ <- mapError StoreErr (saveAllInStore (scRootPath cfg) (prjStore env))
  stm <- liftIO (STM.newTVarIO (prjStore env))
  pure (MimsaEnvironment stm cfg)

getDefaultProject :: ServerM (Error Annotation) (Project Annotation)
getDefaultProject =
  ( do
      env <- mapError StoreErr loadProject
      let items = length . getStore . prjStore $ env
      logInfoN $ "Successfully loaded project, " <> T.pack (show items) <> " store items found"
      pure env
  )
    `catchError` \_ -> do
      logInfoN "Failed to load project, loading default project"
      pure stdlib

server :: IO ()
server = do
  _ <- register ghcMetrics
  mimsaConfig' <- runExceptT getMimsaEnv
  case mimsaConfig' of
    Left e -> putStrLn e >> pure ()
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
