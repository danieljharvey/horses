module Language.Mimsa.Interpreter.Types where

import Control.Monad.Except
import Control.Monad.Trans.State.Lazy
import Language.Mimsa.Types

type App = StateT Scope (ExceptT InterpreterError IO)
