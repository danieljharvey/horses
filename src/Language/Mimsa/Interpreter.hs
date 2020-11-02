module Language.Mimsa.Interpreter
  ( interpret,
  )
where

-- let's run our code, at least for the repl
-- run == simplify, essentially
import Control.Monad.Reader
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as M
import Language.Mimsa.Interpreter.Interpret
import Language.Mimsa.Types

interpret ::
  (Eq ann, Monoid ann) =>
  Scope ann ->
  Swaps ->
  Expr Variable ann ->
  Either (InterpreterError ann) (Expr Variable ann)
interpret scope' swaps expr = fst <$> either'
  where
    scopeSize = M.size . getScope
    either' =
      runReaderT
        ( runStateT
            (interpretWithScope expr)
            (scopeSize scope', scope')
        )
        swaps
