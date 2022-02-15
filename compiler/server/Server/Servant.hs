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
import Server.Compile
import Server.Expression
import Server.Project
import Server.Search
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
