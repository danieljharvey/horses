{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.MimsaConfig
  ( MimsaConfig (..),
  )
where

import GHC.Generics

data MimsaConfig = MimsaConfig
  { port :: Int, -- "PORT",
    storeRootPath :: String,
    showLogs :: Bool,
    prometheusPort :: Maybe Int
  }
  deriving stock (Generic, Eq, Ord, Show)
