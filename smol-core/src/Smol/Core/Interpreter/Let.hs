module Smol.Core.Interpreter.Let (interpretLet) where

import Smol.Core.Interpreter.FindUses
import Smol.Core.Interpreter.Monad
import Smol.Core.Interpreter.Types
import Smol.Core.Interpreter.Types.Stack
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.ResolvedDep

-- need to interpret the expr in the let binding
-- BUT it needs to refer to itself
-- this is NOT the one, we need some form of indirection so the closure can say
-- "and look up whatever 'var' is pls"
interpretLetExpr ::
  (Monoid ann) =>
  InterpretFn ann ->
  ResolvedDep Identifier ->
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretLetExpr interpretFn var expr = do
  intExpr <- interpretFn expr
  case intExpr of
    lambdaExpr@ELambda {} ->
      if isRecursive var lambdaExpr
        then -- make this a function of \binding -> actualFunction
          interpretFn (ELambda (ExprData mempty True mempty) var lambdaExpr)
        else -- non-recursive, run as normal
          interpretFn lambdaExpr
    _ -> pure intExpr

interpretLet ::
  (Monoid ann) =>
  InterpretFn ann ->
  (ResolvedDep Identifier, ExprData ann) ->
  InterpretExpr ann ->
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretLet interpretFn ident expr body = do
  -- calc expr, including itself to sort recursion
  intExpr <-
    interpretLetExpr
      interpretFn
      (fst ident)
      expr

  -- calc rest, with new binding added to the current stack frame
  extendStackFrame
    [(fst ident, intExpr)]
    (interpretFn body)

isRecursive :: ResolvedDep Identifier -> Expr ResolvedDep ann -> Bool
isRecursive var expr =
  memberInUses var (findUses expr)
