{-# LANGUAGE OverloadedStrings #-}

module Server.EnvVars
  ( getMimsaEnv,
  )
where

import Control.Monad.Except
import Data.Maybe
import Language.Mimsa.Types.Store.RootPath
import Server.ServerConfig
import System.Environment

-- manually parse env vars because yolo
getMimsaEnv :: (MonadIO m) => m ServerConfig
getMimsaEnv =
  ServerConfig
    <$> getItem "PORT" read 8999
    <*> getItem "STORE_ROOT_PATH" (pure . RootPath) (RootPath "./.mimsa")
    <*> getItem "SHOW_LOGS" read False
    <*> getItem "PROMETHEUS_PORT" read Nothing

getItem :: (MonadIO m) => String -> (String -> Maybe a) -> a -> m a
getItem envName parse def = do
  item <- liftIO (lookupEnv envName)
  pure (fromMaybe def (item >>= parse))
