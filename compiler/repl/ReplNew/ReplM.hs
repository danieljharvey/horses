{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module ReplNew.ReplM
  ( ReplM (..),
    ReplState (..),
    runReplM,
    mapError,
    setStoredModule,
    getStoredModule,
    replMFromEither,
    replOutput,
    replDocOutput,
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
import Control.Monad.State
import qualified Data.Text.IO as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Typechecker
import Prettyprinter
import ReplNew.Types

-- | to allow us to do 'bindings' in the repl,
-- we maintain a current Module and add to it
newtype ReplState = ReplState
  { rsModule :: Module MonoType
  }

-- | Although we are lucky and can keep much of our work
-- outside of IO, we do need to do some Serious Business sometimes
-- so here is a Monad to do it in
newtype ReplM e a = ReplM
  { getReplM ::
      ExceptT
        e
        ( LoggingT
            ( ReaderT
                ReplConfig
                (StateT ReplState IO)
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
      MonadState ReplState,
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
      emptyState = ReplState mempty
   in evalStateT
        ( runReaderT
            ( runStdoutLoggingT
                (logFilter config innerApp)
            )
            config
        )
        emptyState

logFilter :: ReplConfig -> LoggingT m a -> LoggingT m a
logFilter mc app = if rcShowLogs mc then app else filterLogger (\_ _ -> False) app

-- | lift Either into ReplM
replMFromEither :: Either e a -> ReplM e a
replMFromEither = ReplM . liftEither

-- | Output stuff for use in repl
replOutput :: (Printer a) => a -> ReplM e ()
replOutput = liftIO . T.putStrLn . prettyPrint

-- | Output a Doc from prettyprinter
replDocOutput :: Doc a -> ReplM e ()
replDocOutput = liftIO . T.putStrLn . renderWithWidth 40

-- | we maintain a module in state, this allows us to update it
setStoredModule :: Module MonoType -> ReplM e ()
setStoredModule newMod =
  modify (\s -> s {rsModule = newMod})

getStoredModule :: ReplM e (Module MonoType)
getStoredModule = gets rsModule
