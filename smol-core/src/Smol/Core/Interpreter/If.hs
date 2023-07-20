module Smol.Core.Interpreter.If (interpretIf) where

import Control.Monad.Except
import Smol.Core.Interpreter.Types
import Smol.Core.Interpreter.Types.InterpreterError
import Smol.Core.Interpreter.Types.Stack
import Smol.Core.Types.Expr
import Smol.Core.Types.Prim

interpretIf ::
  InterpretFn ann ->
  ExprData ann ->
  InterpretExpr ann ->
  InterpretExpr ann ->
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretIf interpretFn ann predicate true false =
  case predicate of
    (EPrim _ (PBool pred')) ->
      if pred'
        then interpretFn true
        else interpretFn false
    all'@EPrim {} ->
      throwError $ PredicateForIfMustBeABoolean all'
    pred' -> do
      predExpr <- interpretFn pred'
      interpretFn (EIf ann predExpr true false)
