module Language.Mimsa.Interpreter.Monad
  ( findOperator,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Language.Mimsa.Core
import Language.Mimsa.Interpreter.ToHOAS
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Store.ExprHash

lookupInGlobals :: ExprHash -> InterpreterM var ann (InterpretExpr var ann)
lookupInGlobals exprHash = do
  globals <- asks ireGlobals
  case M.lookup exprHash globals of
    Just found -> pure found
    _ -> throwError (CouldNotFindGlobal (fromHOAS <$> globals) exprHash)

-- lookup custom infixOp in global scope
findOperator :: InfixOp -> InterpreterM var ann (InterpretExpr var ann)
findOperator infixOp = do
  allInfixes <- asks ireInfixes
  case M.lookup infixOp allInfixes of
    Just infixHash -> lookupInGlobals infixHash
    _ -> throwError (CouldNotFindInfix infixOp)
