{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module ReplNew.ReplM
  ( ReplM (..),
    runReplM,
    mapError,
    replMFromEither,
    replOutput,
  )
where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
  ( LoggingT,
    MonadLogger,
    filterLogger,
    runStdoutLoggingT,
  )
import Control.Monad.Reader
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import ReplNew.Types

-- | Although we are lucky and can keep much of our work
-- outside of IO, we do need to do some Serious Business sometimes
-- so here is a Monad to do it in
newtype ReplM e a = ReplM
  { getReplM ::
      ExceptT
        e
        ( LoggingT
            ( ReaderT ReplConfig IO
            )
        )
        a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader ReplConfig,
      MonadError e,
      MonadLogger,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

-- | change error type
mapError :: (e -> e') -> ReplM e a -> ReplM e' a
mapError f = ReplM . withExceptT f . getReplM

-- | run this big brave boy
runReplM :: ReplConfig -> ReplM e a -> IO (Either e a)
runReplM config app =
  let innerApp = runExceptT (getReplM app)
   in runReaderT (runStdoutLoggingT (logFilter config innerApp)) config

logFilter :: ReplConfig -> LoggingT m a -> LoggingT m a
logFilter mc app = if rcShowLogs mc then app else filterLogger (\_ _ -> False) app

-- | lift Either into ReplM
replMFromEither :: Either e a -> ReplM e a
replMFromEither = ReplM . liftEither

-- | Output stuff for use in repl
replOutput :: (Printer a) => a -> ReplM e ()
replOutput = liftIO . T.putStrLn . prettyPrint
