module Language.Mimsa.Typechecker.TcMonad where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (State)
import Language.Mimsa.Types

type TcMonad = ExceptT TypeError (ReaderT Swaps (State Int))
