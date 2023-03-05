{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Server.ServerM
  ( ServerM (..),
    runServerM,
    mapError,
    mimsaFromEither,
    replOutput,
  )
where

import Control.Monad.Catch
import Control.Monad.Except hiding (mapError)
import Control.Monad.Logger
  ( LoggingT,
    MonadLogger,
    filterLogger,
    runStdoutLoggingT,
  )
import Control.Monad.Reader
import qualified Data.Text.IO as T
import Language.Mimsa.Core
import Server.ServerConfig

-- | Although we are lucky and can keep much of our work
-- outside of IO, we do need to do some Serious Business sometimes
-- so here is a Monad to do it in
newtype ServerM e a = ServerM
  { getServerM ::
      ExceptT
        e
        ( LoggingT
            ( ReaderT ServerConfig IO
            )
        )
        a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader ServerConfig,
      MonadError e,
      MonadLogger,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

-- | change error type
mapError :: (e -> e') -> ServerM e a -> ServerM e' a
mapError f = ServerM . withExceptT f . getServerM

-- | run this big brave boy
runServerM :: ServerConfig -> ServerM e a -> IO (Either e a)
runServerM config app =
  let innerApp = runExceptT (getServerM app)
   in runReaderT (runStdoutLoggingT (logFilter config innerApp)) config

logFilter :: ServerConfig -> LoggingT m a -> LoggingT m a
logFilter mc app = if scShowLogs mc then app else filterLogger (\_ _ -> False) app

-- | lift Either into ServerM
mimsaFromEither :: Either e a -> ServerM e a
mimsaFromEither = ServerM . liftEither

-- | Output stuff for use in repl
replOutput :: (Printer a) => a -> ServerM e ()
replOutput = liftIO . T.putStrLn . prettyPrint
