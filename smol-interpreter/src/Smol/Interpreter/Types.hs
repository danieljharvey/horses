{-# LANGUAGE DerivingStrategies #-}

module Smol.Interpreter.Types
  ( InterpreterM,
    InterpretExpr,
    InterpretFn,
    InterpretReaderEnv (..),
    InterpretPattern,
  )
where

import Control.Monad.Reader
import Data.Map.Strict (Map)
import Smol.Interpreter.Types.InterpreterError
import Smol.Interpreter.Types.Stack
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.Pattern
import Smol.Core.Types.ResolvedDep

type InterpreterM ann a =
  ReaderT
    (InterpretReaderEnv ann)
    (Either (InterpreterError ann))
    a

data InterpretReaderEnv ann = InterpretReaderEnv
  { ireStack :: StackFrame ann,
    ireGlobals :: Map (ResolvedDep Identifier) (InterpretExpr ann)
  }

type InterpretExpr ann = Expr ResolvedDep (ExprData ann)

type InterpretPattern ann =
  Pattern ResolvedDep (ExprData ann)

type InterpretFn ann =
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
