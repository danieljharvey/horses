module Language.Mimsa.Interpreter.Main
  ( interpret,
  )
where

-- let's run our code, at least for the repl
-- run == simplify, essentially

import Control.Monad.Except
import Control.Monad.Trans.State.Lazy
import Language.Mimsa.Interpreter.HighestVar
import Language.Mimsa.Interpreter.Interpret
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Interpreter.UseSwaps
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.Swaps

interpret ::
  (Eq ann, Monoid ann) =>
  Scope ann ->
  Swaps ->
  Expr Variable ann ->
  Either (InterpreterError ann) (Expr Name ann)
interpret scope' swaps expr =
  case fst interpretOutput of
    Left e -> throwError e
    Right expr' -> useSwaps (isSwaps (snd interpretOutput)) expr'
  where
    initialState =
      InterpretState
        { isVarNum = highestVar expr + 1,
          isScope = scope',
          isInfix = mempty,
          isApplyCount = 0,
          isSwaps = swaps
        }
    fn = interpretWithScope expr
    interpretOutput =
      runState (runExceptT (getApp fn)) initialState
