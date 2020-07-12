{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.InterpreterError where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Printer
import Language.Mimsa.Types.Store

data InterpreterError
  = UnknownInterpreterError
  | CouldNotFindVar Scope Name
  | CouldNotFindBuiltIn Scope Name
  | CannotDestructureAsPair (Expr Name)
  | CannotDestructureAsSum (Expr Name)
  | CannotDestructureAsRecord (Expr Name) Name
  | CannotDestructureAsList (Expr Name)
  | CannotApplyToNonFunction (Expr Name)
  | CannotFindMemberInRecord (Map Name (Expr Name)) Name
  | PredicateForIfMustBeABoolean (Expr Name)
  deriving (Eq, Ord, Show)

instance Printer InterpreterError where
  prettyPrint (CouldNotFindVar _ name) = "Could not find var " <> prettyPrint name
  prettyPrint (CouldNotFindBuiltIn _ name) = "Could not find built-in " <> prettyPrint name
  prettyPrint (CannotDestructureAsPair expr) = "Expected a pair. Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotDestructureAsSum expr) = "Expected a sum type. Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotDestructureAsRecord expr name) = "Expected a record with a member " <> prettyPrint name <> ". Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotDestructureAsList expr) = "Expected a list. Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotApplyToNonFunction expr) = "Expected a function. Cannot apply a value to " <> prettyPrint expr
  prettyPrint (CannotFindMemberInRecord items name) = "Could not find member " <> prettyPrint name <> " in " <> list
    where
      list = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys items) <> " ]"
  prettyPrint (PredicateForIfMustBeABoolean expr) = "Expected a boolean as a predicate. Cannot use: " <> prettyPrint expr
  prettyPrint UnknownInterpreterError = "Unknown interpreter error"

instance Semigroup InterpreterError where
  a <> _ = a

instance Monoid InterpreterError where
  mempty = UnknownInterpreterError
