module Language.Mimsa.Interpreter.If (interpretIf) where

import Control.Monad.Except
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Interpreter.Stack

interpretIf ::
  InterpretFn var ann ->
  ExprData var ann ->
  InterpretExpr var ann ->
  InterpretExpr var ann ->
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretIf interpretFn ann predicate true false =
  case predicate of
    (MyLiteral _ (MyBool pred')) ->
      if pred'
        then interpretFn true
        else interpretFn false
    all'@MyLiteral {} ->
      throwError $ PredicateForIfMustBeABoolean all'
    all'@MyLambda {} ->
      throwError $ PredicateForIfMustBeABoolean all'
    pred' -> do
      predExpr <- interpretFn pred'
      interpretFn (MyIf ann predExpr true false)
