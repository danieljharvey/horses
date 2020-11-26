{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Type
  ( server,
    MimsaAPI,
  )
where

import Control.Monad.Except
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Server.EnvVars (MimsaConfig (..), getMimsaEnv)
import Language.Mimsa.Server.Project
import Language.Mimsa.Server.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant

type MimsaAPI = ProjectAPI :<|> StoreAPI

mimsaAPI :: Proxy MimsaAPI
mimsaAPI = Proxy

mimsaServer :: Project Annotation -> Server MimsaAPI
mimsaServer prj = projectEndpoints prj :<|> storeEndpoints

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
mimsaApp :: Project Annotation -> Application
mimsaApp prj =
  corsMiddleware $ serve mimsaAPI (mimsaServer prj)

getDefaultProject :: IO (Project Annotation)
getDefaultProject = do
  loadedEnv <- runExceptT loadProject
  case loadedEnv of
    Right env' -> do
      let items = length . getStore . store $ env'
      T.putStrLn $ "Successfully loaded project, " <> T.pack (show items) <> " store items found"
      pure env'
    _ -> do
      T.putStrLn "Failed to load project, loading default project"
      pure defaultProject

server :: IO ()
server = do
  mimsaConfig <- runExceptT getMimsaEnv
  case mimsaConfig of
    Left e -> error e
    Right cfg -> do
      prj <- getDefaultProject
      let port' = port cfg
      T.putStrLn $ "Starting server on port " <> prettyPrint port' <> "..."
      run port' (mimsaApp prj)
