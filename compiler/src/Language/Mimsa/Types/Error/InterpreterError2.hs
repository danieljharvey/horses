{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.InterpreterError2 (InterpreterError2 (..)) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers.Name
import Language.Mimsa.Types.Interpreter.Stack

data InterpreterError2 var ann
  = UnknownInterpreterError2
  | CouldNotFindVar (Map var (Expr var (StackFrame var ann))) var
  | AdditionWithNonNumber (Expr var (StackFrame var ann))
  | SubtractionWithNonNumber (Expr var (StackFrame var ann))
  | ComparisonWithNonNumber Operator (Expr var (StackFrame var ann))
  | StringConcatenationFailure (Expr var (StackFrame var ann)) (Expr var (StackFrame var ann))
  | ArrayConcatenationFailure (Expr var (StackFrame var ann)) (Expr var (StackFrame var ann))
  | PredicateForIfMustBeABoolean (Expr var (StackFrame var ann))
  | CannotDestructureAsRecord (Expr var (StackFrame var ann)) Name
  | CannotFindMemberInRecord (Map Name (Expr var (StackFrame var ann))) Name
  | PatternMatchFailure (Expr var (StackFrame var ann))
  deriving stock (Eq, Ord, Show)

instance Semigroup (InterpreterError2 var ann) where
  a <> _ = a

instance Monoid (InterpreterError2 var ann) where
  mempty = UnknownInterpreterError2

instance (Show ann, Show var, Printer ann, Printer var) => Printer (InterpreterError2 var ann) where
  prettyPrint (CouldNotFindVar items name) =
    "Could not find var " <> prettyPrint name <> " in " <> itemList
    where
      itemList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys items) <> " ]"
  prettyPrint UnknownInterpreterError2 = "Unknown interpreter 2 error"
  prettyPrint (AdditionWithNonNumber a) =
    "Addition expected number but got this: " <> T.pack (show a)
  prettyPrint (SubtractionWithNonNumber a) =
    "Subtraction expected number but got this: " <> T.pack (show a)
  prettyPrint (ComparisonWithNonNumber op a) =
    "Operator " <> prettyPrint op <> " expected number but got this: " <> T.pack (show a)
  prettyPrint (StringConcatenationFailure a b) =
    "Concatenation expected string + string but got this: " <> T.pack (show a) <> " and " <> T.pack (show b)
  prettyPrint (ArrayConcatenationFailure a b) =
    "Concatenation expected array + array but got this: " <> T.pack (show a) <> " and " <> T.pack (show b)
  prettyPrint (PredicateForIfMustBeABoolean expr) =
    "Expected a boolean as a predicate. Cannot use: " <> T.pack (show expr)
  prettyPrint (CannotDestructureAsRecord expr name) =
    "Expected a record with a member " <> prettyPrint name <> ". Cannot destructure: " <> T.pack (show expr)
  prettyPrint (CannotFindMemberInRecord items name) =
    "Could not find member " <> prettyPrint name <> " in " <> itemList
    where
      itemList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys items) <> " ]"
  prettyPrint (PatternMatchFailure expr') =
    "Could not pattern match on value " <> T.pack (show expr')
