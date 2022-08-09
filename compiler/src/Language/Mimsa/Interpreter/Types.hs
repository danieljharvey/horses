{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Interpreter.Types
  ( InterpreterM,
    InterpretExpr,
    InterpretFn,
    InterpretReaderEnv (..),
    InterpretPattern,
  )
where

import Control.Monad.Reader
import Data.Map.Strict (Map)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Interpreter.Stack
import Language.Mimsa.Types.Store.ExprHash
import Language.Mimsa.Types.Typechecker.Unique

type InterpreterM var ann a =
  ReaderT
    (InterpretReaderEnv var ann)
    (Either (InterpreterError var ann))
    a

data InterpretReaderEnv var ann = InterpretReaderEnv
  { ireStack :: StackFrame var ann,
    ireGlobals :: Map ExprHash (InterpretExpr var ann),
    ireInfixes :: Map InfixOp ExprHash
  }

type InterpretExpr var ann = Expr (var, Unique) (ExprData var ann)

type InterpretPattern var ann =
  Pattern (var, Unique) (ExprData var ann)

type InterpretFn var ann =
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
