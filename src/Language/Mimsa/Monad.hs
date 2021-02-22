{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.Mimsa.Monad
  ( MimsaM (..),
    runMimsaM,
    getMimsaConfig,
    logDebug,
    logInfo,
    logError,
    mapError,
    mimsaFromEither,
    replOutput,
  )
where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
  ( LoggingT,
    MonadLogger,
    filterLogger,
    logDebugN,
    logErrorN,
    logInfoN,
    runStdoutLoggingT,
  )
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.MimsaConfig

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
  let innerApp = runExceptT (getMimsaM app)
   in runReaderT (runStdoutLoggingT (logFilter config innerApp)) config

logFilter :: MimsaConfig -> LoggingT m a -> LoggingT m a
logFilter mc app = if showLogs mc then app else filterLogger (\_ _ -> False) app

-- | lift Either into MimsaM
mimsaFromEither :: Either e a -> MimsaM e a
mimsaFromEither = MimsaM . liftEither

-- | Output stuff for use in repl
replOutput :: (Printer a) => a -> MimsaM e ()
replOutput = liftIO . T.putStrLn . prettyPrint

-- | Logging
logDebug :: Text -> MimsaM e ()
logDebug = logDebugN

logInfo :: Text -> MimsaM e ()
logInfo = logInfoN

logError :: Text -> MimsaM e ()
logError = logErrorN
