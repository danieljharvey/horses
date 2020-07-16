module Language.Mimsa.Typechecker.TcMonad where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (State, get, put)
import Language.Mimsa.Types

type TcMonad = ExceptT TypeError (ReaderT Swaps (State UniVar))

getNextUniVar :: TcMonad UniVar
getNextUniVar = do
  nextUniVar <- get
  put (nextUniVar + 1)
  pure nextUniVar

getUnknown :: TcMonad MonoType
getUnknown = MTVar . TVFree <$> getNextUniVar

getBoundType :: TcMonad MonoType
getBoundType = MTVar . TVBound <$> getNextUniVar
