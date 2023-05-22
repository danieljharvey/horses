{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.Servant
  ( MimsaAPI,
    mimsaAPI,
    mimsaServer,
  )
where

import Data.Proxy
import Servant
import Server.Endpoints.Compile
import Server.Endpoints.Module
import Server.Endpoints.Project
import Server.Types

type MimsaAPI =
  ProjectAPI
    :<|> CompileAPI
    :<|> ModuleAPI

mimsaAPI :: Proxy MimsaAPI
mimsaAPI = Proxy

mimsaServer :: MimsaEnvironment -> Server MimsaAPI
mimsaServer mimsaEnv =
  projectEndpoints mimsaEnv
    :<|> compileEndpoints mimsaEnv
    :<|> moduleEndpoints mimsaEnv