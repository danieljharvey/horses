module Language.Mimsa.Interpreter2.Let (interpretLet) where

import Language.Mimsa.Interpreter2.Monad
import Language.Mimsa.Interpreter2.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Interpreter.Stack
import Language.Mimsa.Types.Store.ExprHash

varFromIdent :: Identifier var ann -> var
varFromIdent (Identifier _ var) = var
varFromIdent (AnnotatedIdentifier _ var) = var

-- need to interpret the expr in the let binding
-- BUT it needs to refer to itself
-- this is NOT the one, we need some form of indirection so the closure can say
-- "and look up whatever 'var' is pls"
interpretLetExpr ::
  (Ord var) =>
  InterpretFn var ann ->
  (var, Maybe ExprHash) ->
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretLetExpr interpretFn var expr = do
  intExpr <- interpretFn expr
  case intExpr of
    lambdaExpr@MyLambda {} ->
      -- make this a function of \binding -> actualFunction
      pure (MyLambda mempty (Identifier mempty var) lambdaExpr)
    other -> pure other

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
      interpretFn
      (varFromIdent ident)
      expr

  -- calc rest, with new binding added to the current stack frame
  addToStackFrame
    (varFromIdent ident)
    intExpr
    (interpretFn body)
