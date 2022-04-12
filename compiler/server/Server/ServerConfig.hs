{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Server.ServerConfig
  ( ServerConfig (..),
  )
where

import GHC.Generics
import Language.Mimsa.Types.Store.RootPath

data ServerConfig = ServerConfig
  { scPort :: Int, -- "PORT",
    scRootPath :: RootPath,
    scShowLogs :: Bool,
    scPrometheusPort :: Maybe Int
  }
  deriving stock (Generic, Eq, Ord, Show)
