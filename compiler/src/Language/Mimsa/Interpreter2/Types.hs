{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Interpreter2.Types (InterpreterM, InterpretExpr, InterpretFn) where

import Control.Monad.Reader
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError2
import Language.Mimsa.Types.Interpreter.Stack

type InterpreterM var ann a =
  ReaderT
    (Stack var ann)
    (Either (InterpreterError2 var ann))
    a

type InterpretExpr var ann = Expr var (StackFrame var ann)

type InterpretFn var ann =
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
