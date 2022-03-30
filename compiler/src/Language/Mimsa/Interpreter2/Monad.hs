module Language.Mimsa.Interpreter2.Monad
  ( withNewStackFrame,
    extendStackFrame,
    getCurrentStackFrame,
    lookupVar,
    addVarToFrame,
    findOperator,
    addOperator,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M
import Language.Mimsa.Interpreter2.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError2
import Language.Mimsa.Types.Interpreter.Stack
import Language.Mimsa.Types.Store.ExprHash

-- | run action with entirely new frame
-- | useful for running functions from their closures
withNewStackFrame ::
  StackFrame var ann ->
  InterpreterM var ann a ->
  InterpreterM var ann a
withNewStackFrame sf =
  local
    (\ire -> ire {ireStack = sf})

extendStackFrame ::
  (Ord var) =>
  [ ( (var, Maybe ExprHash),
      InterpretExpr var ann
    )
  ] ->
  InterpreterM var ann a ->
  InterpreterM var ann a
extendStackFrame bindings =
  local
    ( \ire ->
        ire
          { ireStack =
              foldr (uncurry addVarToFrame) (ireStack ire) bindings
          }
    )

getCurrentStackFrame :: InterpreterM var ann (StackFrame var ann)
getCurrentStackFrame = asks ireStack

lookupInGlobals :: ExprHash -> InterpreterM var ann (InterpretExpr var ann)
lookupInGlobals exprHash = do
  globals <- asks ireGlobals
  case M.lookup exprHash globals of
    Just found -> pure found
    _ -> throwError (CouldNotFindGlobal globals exprHash)

lookupVar ::
  (Ord var, Monoid ann) =>
  (var, Maybe ExprHash) ->
  InterpreterM var ann (InterpretExpr var ann)
lookupVar (var, maybeExprHash) =
  case maybeExprHash of
    Just exprHash -> do
      intExpr <- lookupInGlobals exprHash
      case intExpr of
        -- if it points to another var, fetch that
        (MyVar _ a) -> lookupVar a
        other -> pure other
    Nothing -> do
      (StackFrame entries _) <- getCurrentStackFrame
      case M.lookup var entries of
        Just myLam@(MyLambda (ExprData _ isRec _) _ _) ->
          -- when we save functions on the stack we save them as
          -- \letName -> function
          -- so that recursion works
          -- therefore when fetching it we apply it to itself
          -- like a fixpoint combinator thing
          if isRec
            then pure (MyApp mempty myLam myLam)
            else pure myLam
        -- if it's another var, fetch that
        Just (MyVar _ a) -> lookupVar a
        -- otherwise return it
        Just other -> pure other
        -- could not find var
        _ -> throwError (CouldNotFindVar entries var)

addOperator :: InfixOp -> InterpretExpr var ann -> InterpreterM var ann a -> InterpreterM var ann a
addOperator infixOp expr = do
  local
    ( \ire ->
        ire
          { ireStack =
              addOperatorToFrame infixOp expr (ireStack ire)
          }
    )

findOperator :: InfixOp -> InterpreterM var ann (InterpretExpr var ann)
findOperator infixOp = do
  (StackFrame _ infixes) <- getCurrentStackFrame
  case M.lookup infixOp infixes of
    Just entry -> pure entry
    _ -> error "could not find op" -- throwError (CouldNotFindVar infixes infixOp)

addOperatorToFrame :: InfixOp -> InterpretExpr var ann -> StackFrame var ann -> StackFrame var ann
addOperatorToFrame infixOp expr (StackFrame entries infixes) =
  StackFrame entries (M.singleton infixOp expr <> infixes)

addVarToFrame ::
  (Ord var) =>
  (var, Maybe ExprHash) ->
  InterpretExpr var ann ->
  StackFrame var ann ->
  StackFrame var ann
addVarToFrame (var, _) expr (StackFrame entries infixes) =
  StackFrame (M.singleton var expr <> entries) infixes
