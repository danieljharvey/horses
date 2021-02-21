{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Mimsa.Monad
  ( MimsaM (..),
    runMimsaM,
    getMimsaConfig,
    logDebug,
    logInfo,
    logError,
    mapError,
    mimsaFromEither,
  )
where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
  ( LoggingT,
    MonadLogger,
    logDebugN,
    logErrorN,
    logInfoN,
    runStdoutLoggingT,
  )
import Control.Monad.Reader
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Text (Text)
import Language.Mimsa.Server.EnvVars
import Language.Mimsa.Types.Error

-- | Although we are lucky and can keep much of our work
-- outside of IO, we do need to do some Serious Business sometimes
-- so here is a Monad to do it in
newtype MimsaM e a = MimsaM
  { getMimsaM ::
      ExceptT
        e
        ( LoggingT
            ( ReaderT MimsaConfig IO
            )
        )
        a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader MimsaConfig,
      MonadError e,
      MonadLogger,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

-- | get env
getMimsaConfig :: MimsaM e MimsaConfig
getMimsaConfig = ask

-- | change error type
mapError :: (e -> e') -> MimsaM e a -> MimsaM e' a
mapError f = MimsaM . withExceptT f . getMimsaM

-- | run this big brave boy
runMimsaM :: MimsaConfig -> MimsaM e a -> IO (Either e a)
runMimsaM config app =
  runReaderT (runStdoutLoggingT (runExceptT (getMimsaM app))) config

-- | lift Either into MimsaM
mimsaFromEither :: Either e a -> MimsaM e a
mimsaFromEither = MimsaM . liftEither

-- | Logging
logDebug :: Text -> MimsaM e ()
logDebug = logDebugN

logInfo :: Text -> MimsaM e ()
logInfo = logInfoN

logError :: Text -> MimsaM e ()
logError = logErrorN
