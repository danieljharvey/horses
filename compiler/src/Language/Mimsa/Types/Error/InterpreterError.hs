{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.InterpreterError (InterpreterError (..)) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Natural
import Language.Mimsa.Core
import Language.Mimsa.Types.Interpreter.Stack
import Language.Mimsa.Types.Store.ExprHash
import Language.Mimsa.Types.Typechecker.Unique

type InterpretExpr var ann = Expr (var, Unique ExprHash) (ExprData var ann)

data InterpreterError var ann
  = UnknownInterpreterError
  | CouldNotFindVar (Map var (InterpretExpr var ann)) var
  | CouldNotFindInfix (Map InfixOp (InterpretExpr var ann)) InfixOp
  | CouldNotFindGlobal (Map ExprHash (InterpretExpr var ann)) ExprHash
  | AdditionWithNonNumber (InterpretExpr var ann)
  | SubtractionWithNonNumber (InterpretExpr var ann)
  | ComparisonWithNonNumber Operator (InterpretExpr var ann)
  | StringConcatenationFailure (InterpretExpr var ann) (InterpretExpr var ann)
  | ArrayConcatenationFailure (InterpretExpr var ann) (InterpretExpr var ann)
  | PredicateForIfMustBeABoolean (InterpretExpr var ann)
  | CannotDestructureAsRecord (InterpretExpr var ann) Name
  | CannotDestructureAsTuple (InterpretExpr var ann) Natural
  | CannotFindMemberInRecord (Map Name (InterpretExpr var ann)) Name
  | CannotFindMemberInTuple [InterpretExpr var ann] Natural
  | PatternMatchFailure (InterpretExpr var ann)
  deriving stock (Eq, Ord, Show)

instance Semigroup (InterpreterError var ann) where
  a <> _ = a

instance Monoid (InterpreterError var ann) where
  mempty = UnknownInterpreterError

instance (Show ann, Show var, Printer ann, Printer var) => Printer (InterpreterError var ann) where
  prettyPrint (CouldNotFindVar items name) =
    "Could not find var " <> prettyPrint name <> " in " <> itemList
    where
      itemList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys items) <> " ]"
  prettyPrint (CouldNotFindInfix items infixOp) =
    "Could not find infix " <> prettyPrint infixOp <> " in " <> itemList
    where
      itemList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys items) <> " ]"
  prettyPrint (CouldNotFindGlobal items name) =
    "Could not find global " <> prettyPrint name <> " in " <> itemList
    where
      itemList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys items) <> " ]"
  prettyPrint UnknownInterpreterError = "Unknown interpreter 2 error"
  prettyPrint (AdditionWithNonNumber a) =
    "Addition expected number but got this: " <> prettyPrint a
  prettyPrint (SubtractionWithNonNumber a) =
    "Subtraction expected number but got this: " <> prettyPrint a
  prettyPrint (ComparisonWithNonNumber op a) =
    "Operator " <> prettyPrint op <> " expected number but got this: " <> prettyPrint a
  prettyPrint (StringConcatenationFailure a b) =
    "Concatenation expected string + string but got this: " <> prettyPrint a <> " and " <> prettyPrint b
  prettyPrint (ArrayConcatenationFailure a b) =
    "Concatenation expected array + array but got this: " <> prettyPrint a <> " and " <> prettyPrint b
  prettyPrint (PredicateForIfMustBeABoolean expr) =
    "Expected a boolean as a predicate. Cannot use: " <> prettyPrint expr
  prettyPrint (CannotDestructureAsRecord expr name) =
    "Expected a record with a member " <> prettyPrint name <> ". Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotDestructureAsTuple expr index) =
    "Expected a tuple with an index at " <> prettyPrint index <> ". Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotFindMemberInRecord items name) =
    "Could not find member " <> prettyPrint name <> " in " <> itemList
    where
      itemList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys items) <> " ]"
  prettyPrint (CannotFindMemberInTuple items index) =
    "Could not find index " <> prettyPrint index <> " in " <> itemList
    where
      itemList = "[ " <> prettyPrint items <> " ]"
  prettyPrint (PatternMatchFailure expr') =
    "Could not pattern match on value " <> prettyPrint expr'
