{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Interpreter.Types.InterpreterError (InterpreterError (..), interpreterErrorDiagnostic) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import GHC.Natural
import qualified Prettyprinter as PP
import Smol.Core.Interpreter.Types.Stack
import Smol.Core.Printer
import Smol.Core.Types.Annotation
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.Op
import Smol.Core.Types.ResolvedDep

type InterpretExpr ann = Expr ResolvedDep (ExprData ann)

data InterpreterError ann
  = UnknownInterpreterError
  | CouldNotFindVar (Map (ResolvedDep Identifier) (InterpretExpr ann)) (ResolvedDep Identifier)
  | AdditionWithNonNumber (InterpretExpr ann)
  | SubtractionWithNonNumber (InterpretExpr ann)
  | ComparisonWithNonNumber Op (InterpretExpr ann)
  | StringConcatenationFailure (InterpretExpr ann) (InterpretExpr ann)
  | ArrayConcatenationFailure (InterpretExpr ann) (InterpretExpr ann)
  | PredicateForIfMustBeABoolean (InterpretExpr ann)
  | CannotDestructureAsRecord (InterpretExpr ann) Identifier
  | CannotDestructureAsTuple (InterpretExpr ann) Natural
  | CannotFindMemberInRecord (Map Identifier (InterpretExpr ann)) Identifier
  | CannotFindMemberInTuple [InterpretExpr ann] Natural
  | PatternMatchFailure (InterpretExpr ann)
  deriving stock (Eq, Ord, Show)

instance Semigroup (InterpreterError ann) where
  a <> _ = a

instance Monoid (InterpreterError ann) where
  mempty = UnknownInterpreterError

commaSep :: (Printer a) => [a] -> PP.Doc ann
commaSep =
  foldMap (\a -> prettyDoc a <> ", ")

interpreterErrorDiagnostic :: InterpreterError Annotation -> Diag.Diagnostic T.Text
interpreterErrorDiagnostic intError =
  Diag.addReport mempty $
    Diag.Err
      Nothing
      (prettyPrint intError)
      []
      []
  where
    prettyPrint :: (Printer a) => a -> T.Text
    prettyPrint = renderWithWidth 40 . prettyDoc

instance Printer (InterpreterError ann) where
  prettyDoc (CouldNotFindVar items name) =
    "Could not find var " <> prettyDoc name <> " in " <> itemList
    where
      itemList = "[ " <> commaSep (M.keys items) <> " ]"
  prettyDoc UnknownInterpreterError = "Unknown interpreter 2 error"
  prettyDoc (AdditionWithNonNumber a) =
    "Addition expected number but got this: " <> prettyDoc a
  prettyDoc (SubtractionWithNonNumber a) =
    "Subtraction expected number but got this: " <> prettyDoc a
  prettyDoc (ComparisonWithNonNumber op a) =
    "Operator " <> prettyDoc op <> " expected number but got this: " <> prettyDoc a
  prettyDoc (StringConcatenationFailure a b) =
    "Concatenation expected string + string but got this: " <> prettyDoc a <> " and " <> prettyDoc b
  prettyDoc (ArrayConcatenationFailure a b) =
    "Concatenation expected array + array but got this: " <> prettyDoc a <> " and " <> prettyDoc b
  prettyDoc (PredicateForIfMustBeABoolean expr) =
    "Expected a boolean as a predicate. Cannot use: " <> prettyDoc expr
  prettyDoc (CannotDestructureAsRecord expr name) =
    "Expected a record with a member " <> prettyDoc name <> ". Cannot destructure: " <> prettyDoc expr
  prettyDoc (CannotDestructureAsTuple expr index) =
    "Expected a tuple with an index at " <> prettyDoc index <> ". Cannot destructure: " <> prettyDoc expr
  prettyDoc (CannotFindMemberInRecord items name) =
    "Could not find member " <> prettyDoc name <> " in " <> itemList
    where
      itemList = "[ " <> commaSep (M.keys items) <> " ]"
  prettyDoc (CannotFindMemberInTuple items index) =
    "Could not find index " <> prettyDoc index <> " in " <> itemList
    where
      itemList = "[ " <> prettyDoc items <> " ]"
  prettyDoc (PatternMatchFailure expr') =
    "Could not pattern match on value " <> prettyDoc expr'
