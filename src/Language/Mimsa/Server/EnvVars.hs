{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Server.EnvVars
  ( getMimsaEnv,
    MimsaConfig (..),
  )
where

import Control.Monad.Except
import GHC.Generics
import System.Envy

data MimsaConfig = MimsaConfig
  { port :: Int, -- "PORT",
    storeRootPath :: String
  }
  deriving (Generic, Show)

-- All fields will be converted to uppercase
getMimsaEnv :: ExceptT String IO MimsaConfig
getMimsaEnv =
  ExceptT $
    runEnv $
      gFromEnvCustom
        defOption
        (Just (MimsaConfig 8999 "~/.mimsa"))
