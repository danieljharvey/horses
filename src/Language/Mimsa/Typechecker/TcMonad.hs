module Language.Mimsa.Typechecker.TcMonad where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (State, get, put, runState)
import Language.Mimsa.Types

type TcMonad = ExceptT TypeError (ReaderT Swaps (State Int))

runTcMonad ::
  Swaps ->
  TcMonad a ->
  Either TypeError a
runTcMonad swaps value =
  fst either'
  where
    either' = runState (runReaderT (runExceptT value) swaps) 1

getNextUniVar :: TcMonad Int
getNextUniVar = do
  nextUniVar <- get
  put (nextUniVar + 1)
  pure nextUniVar

getUnknown :: TcMonad MonoType
getUnknown = MTVar mempty . NumberedVar <$> getNextUniVar
