module Language.Mimsa.Interpreter.Monad
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
import qualified Data.Map.Strict as M
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Interpreter.Stack
import Language.Mimsa.Types.Store.ExprHash
import Language.Mimsa.Types.Typechecker.Unique

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
  [ ( (var, Unique),
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
  (Ord var, Monoid ann, Show var, Show ann) =>
  (var, Unique) ->
  InterpreterM var ann (InterpretExpr var ann)
lookupVar (var, maybeExprHash) =
  case maybeExprHash of
    Dependency exprHash -> do
      intExpr <- lookupInGlobals exprHash
      case intExpr of
        -- if it points to another var, fetch that
        (MyVar _ Nothing a) -> lookupVar a
        other -> pure other
    _ -> do
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
        Just (MyVar _ Nothing a) -> lookupVar a
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

-- lookup custom infixOp in stack and then global scope
findOperator :: InfixOp -> InterpreterM var ann (InterpretExpr var ann)
findOperator infixOp = do
  (StackFrame _ infixes) <- getCurrentStackFrame
  case M.lookup infixOp infixes of
    Just entry -> pure entry
    _ -> do
      allInfixes <- asks ireInfixes
      case M.lookup infixOp allInfixes of
        Just infixHash -> lookupInGlobals infixHash
        _ -> throwError (CouldNotFindInfix infixes infixOp)

addOperatorToFrame :: InfixOp -> InterpretExpr var ann -> StackFrame var ann -> StackFrame var ann
addOperatorToFrame infixOp expr (StackFrame entries infixes) =
  StackFrame entries (M.singleton infixOp expr <> infixes)

addVarToFrame ::
  (Ord var) =>
  (var, Unique) ->
  InterpretExpr var ann ->
  StackFrame var ann ->
  StackFrame var ann
addVarToFrame (var, _) expr (StackFrame entries infixes) =
  StackFrame (M.singleton var expr <> entries) infixes
