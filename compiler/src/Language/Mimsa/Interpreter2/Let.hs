module Language.Mimsa.Interpreter2.Let (interpretLet) where

import Language.Mimsa.Interpreter2.Monad
import Language.Mimsa.Interpreter2.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Interpreter.Stack
import Language.Mimsa.Types.Store.ExprHash

varFromIdent :: Identifier var ann -> var
varFromIdent (Identifier _ var) = var
varFromIdent (AnnotatedIdentifier _ var) = var

-- so we need to interpret the expr in a let
-- if there's no recursion we use `interpretFn`
-- if there is (the main thing will be a function)
-- then we need to work out what to use as the closure
-- i _think_ just M.singleton bindingName expr

interpretLetExpr :: (var, Maybe ExprHash) -> InterpretExpr var ann -> InterpreterM var ann (InterpretExpr var ann)
interpretLetExpr _var _expr =
  error "interpretLetExpr broken"

interpretLet ::
  (Ord var) =>
  InterpretFn var ann ->
  Identifier (var, Maybe ExprHash) (StackFrame var ann) ->
  InterpretExpr var ann ->
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretLet interpretFn ident expr body = do
  -- calc expr, including itself to sort recursion
  intExpr <-
    interpretLetExpr
      (varFromIdent ident)
      expr

  -- calc rest, with new binding added to the current stack frame
  addToStackFrame
    (varFromIdent ident)
    intExpr
    (interpretFn body)
