module Language.Mimsa.Typechecker.TcMonad where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (State, get, put, runState)
import Language.Mimsa.Types

type TcMonad a = ExceptT (TypeError a) (ReaderT Swaps (State Int))

runTcMonad ::
  Swaps ->
  TcMonad ann a ->
  Either (TypeError ann) a
runTcMonad swaps value =
  fst either'
  where
    either' = runState (runReaderT (runExceptT value) swaps) 1

getNextUniVar :: TcMonad ann Int
getNextUniVar = do
  nextUniVar <- get
  put (nextUniVar + 1)
  pure nextUniVar

getUnknown :: TcMonad ann MonoType
getUnknown = MTVar . NumberedVar <$> getNextUniVar
