{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
