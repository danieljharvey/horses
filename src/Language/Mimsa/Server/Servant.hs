{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Servant
  ( MimsaAPI,
    mimsaAPI,
    mimsaServer,
  )
where

import Data.Proxy
import Language.Mimsa.Server.Project
import Language.Mimsa.Server.Store
import Language.Mimsa.Server.Types
import Servant

type MimsaAPI = ProjectAPI :<|> StoreAPI

mimsaAPI :: Proxy MimsaAPI
mimsaAPI = Proxy

mimsaServer :: MimsaEnvironment -> Server MimsaAPI
mimsaServer mimsaEnv =
  projectEndpoints mimsaEnv
    :<|> storeEndpoints mimsaEnv
