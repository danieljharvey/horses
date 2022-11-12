module Language.Mimsa.Interpreter.If (interpretIf) where

import Control.Monad.Except
import Language.Mimsa.Core
import Language.Mimsa.Interpreter.ToHOAS
import Language.Mimsa.Interpreter.Types
import qualified Language.Mimsa.Types.AST.HOASExpr as HOAS
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Interpreter.Stack

interpretIf ::
  InterpretFn var ann ->
  ExprData ann ->
  InterpretExpr var ann ->
  InterpretExpr var ann ->
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretIf interpretFn ann predicate true false =
  case predicate of
    (HOAS.MyLiteral _ (MyBool pred')) ->
      if pred'
        then interpretFn true
        else interpretFn false
    all'@HOAS.MyLiteral {} ->
      throwError $ PredicateForIfMustBeABoolean (fromHOAS all')
    all'@HOAS.MyLambda {} ->
      throwError $ PredicateForIfMustBeABoolean (fromHOAS all')
    pred' -> do
      predExpr <- interpretFn pred'
      interpretFn (HOAS.MyIf ann predExpr true false)
