module Language.Mimsa.Interpreter.Types where

import Control.Monad.Except
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor (bimap, first)
import Debug.Trace
import Language.Mimsa.Types

type App = StateT (Int, Scope) (ExceptT InterpreterError IO)

readScope :: App Scope
readScope = gets snd

nextInt :: App Int
nextInt = do
  int' <- gets fst
  modify $ first (1 +)
  pure (traceShowId int')

nextVariable :: App Variable
nextVariable = NumberedVar <$> nextInt

addToScope :: Scope -> App ()
addToScope scope' = modify $ bimap (1 +) (scope' <>)
