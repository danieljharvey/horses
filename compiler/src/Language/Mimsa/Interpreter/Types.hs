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
import Language.Mimsa.Core
import qualified Language.Mimsa.Types.AST.HOASExpr as HOAS
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Identifiers.Name
import Language.Mimsa.Types.Store.ExprHash
import Language.Mimsa.Types.Typechecker.Unique

type InterpreterM ann a =
  ReaderT
    (InterpretReaderEnv ann)
    (Either (InterpreterError Name ann))
    a

data InterpretReaderEnv ann = InterpretReaderEnv
  { ireGlobals :: Map ExprHash (InterpretExpr ann),
    ireInfixes :: Map InfixOp ExprHash
  }

type InterpretExpr ann = HOAS.HOASExpr (Name, Unique) ann

type InterpretPattern ann =
  Pattern (Name, Unique) ann

type InterpretFn ann =
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
