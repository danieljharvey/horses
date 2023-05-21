{-# LANGUAGE OverloadedStrings #-}

module Server.EnvVars
  ( getMimsaEnv,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char
import Data.Maybe
import Language.Mimsa.Types.Store.RootPath
import Server.ServerConfig
import System.Environment
import Text.Read

-- manually parse env vars because yolo
getMimsaEnv :: (MonadIO m) => m ServerConfig
getMimsaEnv =
  ServerConfig
    <$> getItem "PORT" readMaybe 8999
    <*> getItem "STORE_ROOT_PATH" (pure . RootPath) (RootPath "./.mimsa")
    <*> getItem "SHOW_LOGS" readBool False
    <*> getItem "PROMETHEUS_PORT" readMaybe Nothing

readBool :: String -> Maybe Bool
readBool str = if map toUpper str == "TRUE" then Just True else Just False

getItem :: (MonadIO m) => String -> (String -> Maybe a) -> a -> m a
getItem envName parse def = do
  item <- liftIO (lookupEnv envName)
  pure (fromMaybe def (item >>= parse))
