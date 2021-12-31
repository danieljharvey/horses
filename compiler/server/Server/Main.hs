{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Main
  ( server,
  )
where

import qualified Control.Concurrent.STM as STM
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.MimsaConfig
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Server.EnvVars (getMimsaEnv)
import Server.Servant
import Server.Types

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
mimsaApp :: MimsaEnvironment -> Application
mimsaApp mimsaEnv =
  corsMiddleware $ serve mimsaAPI (mimsaServer mimsaEnv)

createMimsaEnvironment :: MimsaM (Error Annotation) MimsaEnvironment
createMimsaEnvironment = do
  cfg <- getMimsaConfig
  env <- getDefaultProject
  _ <- mapError StoreErr (saveAllInStore (prjStore env))
  stm <- liftIO (STM.newTVarIO (prjStore env))
  pure (MimsaEnvironment stm cfg)

getDefaultProject :: MimsaM (Error Annotation) (Project Annotation)
getDefaultProject =
  ( do
      env <- mapError StoreErr loadProject
      let items = length . getStore . prjStore $ env
      logInfo $ "Successfully loaded project, " <> T.pack (show items) <> " store items found"
      pure env
  )
    `catchError` \_ -> do
      logInfo "Failed to load project, loading default project"
      pure stdlib

server :: IO ()
server = do
  mimsaConfig' <- runExceptT getMimsaEnv
  case mimsaConfig' of
    Left e -> putStrLn e >> pure ()
    Right cfg -> do
      rtn <- runMimsaM cfg serverM
      case rtn of
        -- error in initialisation
        Left e -> T.putStrLn (prettyPrint e)
        _ -> pure ()

serverM :: MimsaM (Error Annotation) ()
serverM = do
  cfg <- getMimsaConfig
  mimsaEnv <- createMimsaEnvironment
  let port' = port cfg
  replOutput $ "Starting server on port " <> prettyPrint port' <> "..."
  liftIO $ run port' (mimsaApp mimsaEnv) -- TODO - hoist Servant to use MimsaM?
