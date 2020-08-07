module Language.Mimsa.Interpreter
  ( interpret,
  )
where

-- let's run our code, at least for the repl
-- run == simplify, essentially
import Control.Monad.Except
import Control.Monad.Trans.State.Lazy
import Language.Mimsa.Interpreter.Interpret
import Language.Mimsa.Types

interpret :: Scope -> Expr Variable -> IO (Either InterpreterError (Expr Variable))
interpret scope' expr = fmap fst <$> either'
  where
    either' =
      runExceptT $
        runStateT
          (interpretWithScope expr)
          scope'
