{-# LANGUAGE DeriveGeneric #-}

module Language.Mimsa.Types.MimsaConfig
  ( MimsaConfig (..),
  )
where

import GHC.Generics

data MimsaConfig = MimsaConfig
  { port :: Int, -- "PORT",
    storeRootPath :: String,
    showLogs :: Bool
  }
  deriving (Generic, Eq, Ord, Show)
