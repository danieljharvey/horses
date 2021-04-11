{-# LANGUAGE OverloadedStrings #-}

module Router.Config (Config (..), readConfig) where

import Data.Functor
import Data.Maybe
import System.Environment (lookupEnv)
import Text.Read

data Config = Config
  { cfgPort :: Int,
    cfgVolumePath :: String,
    cfgMimsaBaseUrl :: String,
    cfgMimsaPort :: Int,
    cfgUseHttps :: Bool
  }

readConfig :: IO Config
readConfig = do
  port <- lookupEnv "PORT"
  volumePath <- lookupEnv "VOLUME_PATH"
  apiBaseUrl <- lookupEnv "MIMSA_BASE_URL"
  apiPort <- lookupEnv "MIMSA_PORT"
  useHttps <- lookupEnv "MIMSA_USE_HTTPS"

  pure
    ( Config
        (fromMaybe 8002 (port >>= readMaybe))
        (fromMaybe "file_volume" volumePath)
        (fromMaybe "mimsa-api.isverymuchmybusiness.com" apiBaseUrl)
        (fromMaybe 80 (apiPort >>= readMaybe))
        (fromMaybe False (useHttps $> True))
    )
