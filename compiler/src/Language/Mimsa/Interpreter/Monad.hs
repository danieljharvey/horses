module Language.Mimsa.Interpreter.Monad
  ( findOperator,
    lookupGlobal,
    withVar,
    lookupVar,
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
import Language.Mimsa.Types.Typechecker.Unique

lookupGlobal ::
  (Name, Unique) ->
  InterpreterM ann (Maybe (InterpretExpr ann))
lookupGlobal (_, Dependency exprHash) = Just <$> lookupInGlobals exprHash
lookupGlobal _ = pure Nothing

lookupVar ::
  (Name, Unique) ->
  InterpreterM ann (Maybe (InterpretExpr ann))
lookupVar var = do
  vars <- asks ireVars
  case M.lookup var vars of
    Just found -> pure (Just found)
    _ -> pure Nothing

withVar ::
  (Name, Unique) ->
  InterpretExpr ann ->
  InterpreterM ann a ->
  InterpreterM ann a
withVar var value =
  local (\ire -> ire {ireVars = ireVars ire <> M.singleton var value})

lookupInGlobals :: ExprHash -> InterpreterM ann (InterpretExpr ann)
lookupInGlobals exprHash = do
  globals <- asks ireGlobals
  case M.lookup exprHash globals of
    Just found -> pure found
    _ -> throwError (CouldNotFindGlobal (fromHOAS <$> globals) exprHash)

-- lookup custom infixOp in global scope
findOperator :: InfixOp -> InterpreterM ann (InterpretExpr ann)
findOperator infixOp = do
  allInfixes <- asks ireInfixes
  case M.lookup infixOp allInfixes of
    Just infixHash -> lookupInGlobals infixHash
    _ -> throwError (CouldNotFindInfix infixOp)
