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
import Server.Endpoints.Expression
import Server.Endpoints.Project
import Server.Endpoints.Search
import Server.Types

type MimsaAPI = ProjectAPI :<|> SearchAPI :<|> CompileAPI :<|> GetExpression

mimsaAPI :: Proxy MimsaAPI
mimsaAPI = Proxy

mimsaServer :: MimsaEnvironment -> Server MimsaAPI
mimsaServer mimsaEnv =
  projectEndpoints mimsaEnv
    :<|> searchEndpoints mimsaEnv
    :<|> compileEndpoints mimsaEnv
    :<|> getExpression mimsaEnv
