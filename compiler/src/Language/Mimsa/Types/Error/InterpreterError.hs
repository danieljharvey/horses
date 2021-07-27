{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.InterpreterError where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Scope

data InterpreterError ann
  = UnknownInterpreterError
  | CouldNotFindVar (Scope ann) Variable
  | CouldNotFindInfixOp InfixOp
  | CannotDestructureAsPair (Expr Variable ann)
  | CannotDestructureAsRecord (Expr Variable ann) Name
  | CannotApplyToNonFunction (Expr Variable ann)
  | CannotFindMemberInRecord (Map Name (Expr Variable ann)) Name
  | PredicateForIfMustBeABoolean (Expr Variable ann)
  | PatternMatchFailure (Expr Variable ann)
  | SelfReferencingBinding Variable
  | AdditionWithNonNumber (Expr Variable ann)
  | SubtractionWithNonNumber (Expr Variable ann)
  | StringConcatenationFailure (Expr Variable ann) (Expr Variable ann)
  | ArrayConcatenationFailure (Expr Variable ann) (Expr Variable ann)
  | TypedHoleFound (Expr Variable ann)
  | CouldNotFindSwapForVariable Variable (Map Variable Name)
  | MaximumCallSizeReached
  deriving stock (Eq, Ord, Show)

instance Semigroup (InterpreterError a) where
  a <> _ = a

instance Monoid (InterpreterError a) where
  mempty = UnknownInterpreterError

instance (Show ann, Printer ann) => Printer (InterpreterError ann) where
  prettyPrint (CouldNotFindVar _ name) =
    "Could not find var " <> prettyPrint name
  prettyPrint (CouldNotFindInfixOp op) =
    "Could not find infixOp " <> prettyPrint op
  prettyPrint (CannotDestructureAsPair expr) =
    "Expected a pair. Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotDestructureAsRecord expr name) =
    "Expected a record with a member " <> prettyPrint name <> ". Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotApplyToNonFunction expr) =
    "Expected a function. Cannot apply a value to " <> prettyPrint expr
  prettyPrint (CannotFindMemberInRecord items name) =
    "Could not find member " <> prettyPrint name <> " in " <> itemList
    where
      itemList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys items) <> " ]"
  prettyPrint (PredicateForIfMustBeABoolean expr) =
    "Expected a boolean as a predicate. Cannot use: " <> prettyPrint expr
  prettyPrint (PatternMatchFailure expr') =
    "Could not pattern match on value " <> prettyPrint expr'
  prettyPrint (SelfReferencingBinding b) =
    "Could not bind variable " <> prettyPrint b <> " to itself."
  prettyPrint (AdditionWithNonNumber a) =
    "Addition expected number but got this: " <> prettyPrint a
  prettyPrint (SubtractionWithNonNumber a) =
    "Subtraction expected number but got this: " <> prettyPrint a
  prettyPrint (StringConcatenationFailure a b) =
    "Concatenation expected string + string but got this: " <> prettyPrint a <> " and " <> prettyPrint b
  prettyPrint (ArrayConcatenationFailure a b) =
    "Concatenation expected array + array but got this: " <> prettyPrint a <> " and " <> prettyPrint b
  prettyPrint (TypedHoleFound a) =
    "Typed hole found " <> prettyPrint a
  prettyPrint (CouldNotFindSwapForVariable var swaps) =
    "Could not find swap for variable " <> prettyPrint var <> " in " <> itemList
    where
      itemList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys swaps) <> " ]"
  prettyPrint MaximumCallSizeReached =
    "Maximum size reached, interpreter aborted. Perhaps you have infinite recursion?"
  prettyPrint UnknownInterpreterError =
    "Unknown interpreter error"
