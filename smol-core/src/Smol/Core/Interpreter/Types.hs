{-# LANGUAGE DerivingStrategies #-}

module Smol.Core.Interpreter.Types
  ( InterpreterM,
    InterpretExpr,
    InterpretFn,
    InterpretReaderEnv (..),
    InterpretPattern,
  )
where

import Control.Monad.Reader
import Data.Map.Strict (Map)
import Smol.Core.Interpreter.Types.InterpreterError
import Smol.Core.Interpreter.Types.Stack
import Smol.Core.Types.ResolvedDep
import Smol.Core.Types.Identifier
import Smol.Core.Types.Expr
import Smol.Core.Types.Pattern

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
