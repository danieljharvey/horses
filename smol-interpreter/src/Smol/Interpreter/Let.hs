module Smol.Interpreter.Let (interpretLet) where

import Smol.Core.FindUses
import Smol.Core.Typecheck.Shared (getExprAnnotation)
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.ResolvedDep
import Smol.Interpreter.Monad
import Smol.Interpreter.Types
import Smol.Interpreter.Types.Stack

-- need to interpret the expr in the let binding
-- BUT it needs to refer to itself
-- this is NOT the one, we need some form of indirection so the closure can say
-- "and look up whatever 'var' is pls"
interpretLetExpr ::
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

          let annotation = edAnnotation (getExprAnnotation lambdaExpr)
           in interpretFn (ELambda (ExprData mempty True annotation) var lambdaExpr)
        else -- non-recursive, run as normal
          interpretFn lambdaExpr
    _ -> pure intExpr

interpretLet ::
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
