module Language.Mimsa.Interpreter2.If (interpretIf) where

import Control.Monad.Except
import Language.Mimsa.Interpreter2.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError2
import Language.Mimsa.Types.Interpreter.Stack

interpretIf ::
  InterpretFn var ann ->
  StackFrame var ann ->
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
