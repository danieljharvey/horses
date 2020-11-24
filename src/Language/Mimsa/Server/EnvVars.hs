{-# LANGUAGE DeriveGeneric #-}

module Language.Mimsa.Server.EnvVars
  ( getMimsaEnv,
    MimsaConfig (..),
  )
where

import Control.Monad.Except
import GHC.Generics
import System.Envy

newtype MimsaConfig
  = MimsaConfig
      { mimsaPort :: Int -- "MIMSA_PORT"
      }
  deriving (Generic, Show)

-- All fields will be converted to uppercase
getMimsaEnv :: ExceptT String IO MimsaConfig
getMimsaEnv =
  ExceptT
    $ runEnv
    $ gFromEnvCustom
      defOption
      (Just (MimsaConfig 6000))
