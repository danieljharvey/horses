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
  | CouldNotFindBuiltIn (Scope ann) Variable
  | CannotDestructureAsPair (Expr Variable ann)
  | CannotDestructureAsSum (Expr Variable ann)
  | CannotDestructureAsRecord (Expr Variable ann) Name
  | CannotDestructureAsList (Expr Variable ann)
  | CannotApplyToNonFunction (Expr Variable ann)
  | CannotFindMemberInRecord (Map Name (Expr Variable ann)) Name
  | PredicateForIfMustBeABoolean (Expr Variable ann)
  | CouldNotUnwrapBuiltIn Variable
  | CouldNotMatchBuiltInId BiIds
  | PatternMatchFailure (Expr Variable ann)
  | SelfReferencingBinding Variable
  deriving (Eq, Ord, Show)

instance Semigroup (InterpreterError a) where
  a <> _ = a

instance Monoid (InterpreterError a) where
  mempty = UnknownInterpreterError

instance (Show ann, Printer ann) => Printer (InterpreterError ann) where
  prettyPrint (CouldNotFindVar _ name) = "Could not find var " <> prettyPrint name
  prettyPrint (CouldNotFindBuiltIn _ name) = "Could not find built-in " <> prettyPrint name
  prettyPrint (CannotDestructureAsPair expr) = "Expected a pair. Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotDestructureAsSum expr) = "Expected a sum type. Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotDestructureAsRecord expr name) = "Expected a record with a member " <> prettyPrint name <> ". Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotDestructureAsList expr) = "Expected a list. Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotApplyToNonFunction expr) = "Expected a function. Cannot apply a value to " <> prettyPrint expr
  prettyPrint (CannotFindMemberInRecord items name) = "Could not find member " <> prettyPrint name <> " in " <> itemList
    where
      itemList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys items) <> " ]"
  prettyPrint (PredicateForIfMustBeABoolean expr) = "Expected a boolean as a predicate. Cannot use: " <> prettyPrint expr
  prettyPrint (CouldNotUnwrapBuiltIn name) = "Could unwrap built-in " <> prettyPrint name
  prettyPrint (CouldNotMatchBuiltInId ids) = "Could not match built in ids " <> prettyPrint ids
  prettyPrint (PatternMatchFailure expr') = "Could not pattern match on value " <> prettyPrint expr'
  prettyPrint (SelfReferencingBinding b) = "Could not bind variable " <> prettyPrint b <> " to itself."
  prettyPrint UnknownInterpreterError = "Unknown interpreter error"
