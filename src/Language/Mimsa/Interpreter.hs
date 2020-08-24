module Language.Mimsa.Interpreter
  ( interpret,
    scopeFromSwaps,
  )
where

-- let's run our code, at least for the repl
-- run == simplify, essentially
import Control.Monad.Except
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as M
import Language.Mimsa.Interpreter.Interpret
import Language.Mimsa.Types

interpret :: Scope -> Expr Variable -> IO (Either InterpreterError (Expr Variable))
interpret scope' expr = fmap fst <$> either'
  where
    scopeSize = M.size . getScope
    either' =
      runExceptT $
        runStateT
          (interpretWithScope expr)
          (scopeSize scope', scope')

-- to allow let recursion we need to tell the interpreter about any
-- swaps we made to put them in scope
scopeFromSwaps :: Swaps -> Scope
scopeFromSwaps swaps = Scope $ MyVar . NamedVar <$> swaps
