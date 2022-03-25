{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Interpreter2.Types (InterpreterM, InterpretExpr, InterpretFn, InterpretReaderEnv (..), InterpretPattern) where

import Control.Monad.Reader
import Data.Map (Map)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError2
import Language.Mimsa.Types.Interpreter.Stack
import Language.Mimsa.Types.Store.ExprHash

type InterpreterM var ann a =
  ReaderT
    (InterpretReaderEnv var ann)
    (Either (InterpreterError2 var ann))
    a

data InterpretReaderEnv var ann = InterpretReaderEnv
  { ireStack :: Stack var ann,
    ireGlobals :: Map ExprHash (InterpretExpr var ann)
  }

type InterpretExpr var ann = Expr (var, Maybe ExprHash) (StackFrame var ann)

type InterpretPattern var ann =
  Pattern (var, Maybe ExprHash) (StackFrame var ann)

type InterpretFn var ann =
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
