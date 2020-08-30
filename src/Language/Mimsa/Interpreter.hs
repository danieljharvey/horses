module Language.Mimsa.Interpreter
  ( interpret,
  )
where

-- let's run our code, at least for the repl
-- run == simplify, essentially
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as M
import Language.Mimsa.Interpreter.Interpret
import Language.Mimsa.Types

interpret :: Scope -> Swaps -> Expr Variable -> IO (Either InterpreterError (Expr Variable))
interpret scope' swaps expr = fmap fst <$> either'
  where
    scopeSize = M.size . getScope
    either' =
      runExceptT $
        runReaderT
          ( runStateT
              (interpretWithScope expr)
              (scopeSize scope', scope')
          )
          swaps
