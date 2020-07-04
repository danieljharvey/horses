{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.TypeError
  ( TypeError (..),
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Printer
import Language.Mimsa.Types.Typechecker

data TypeError
  = UnknownTypeError
  | FailsOccursCheck Name
  | UnificationError MonoType MonoType
  | VariableNotInEnv Name Environment
  | MissingRecordMember Name (Map Name Expr)
  | MissingRecordTypeMember Name (Map Name MonoType)
  | MissingBuiltIn Name
  | CannotMatchRecord Environment MonoType
  | CaseMatchExpectedSum MonoType
  | CaseMatchExpectedPair MonoType
  | CaseMatchExpectedList MonoType
  | CaseMatchExpectedLambda Expr Expr
  deriving (Eq, Ord, Show)

showKeys :: Map Name a -> Text
showKeys record = T.intercalate ", " (prettyPrint <$> M.keys record)

instance Printer TypeError where
  prettyPrint UnknownTypeError =
    "Unknown type error"
  prettyPrint (FailsOccursCheck name) =
    prettyPrint name <> " fails occurs check"
  prettyPrint (UnificationError a b) =
    "Unification error - cannot match " <> prettyPrint a <> " and " <> prettyPrint b
  prettyPrint (VariableNotInEnv name env) =
    "Variable " <> prettyPrint name <> " not in scope: { " <> showKeys env <> " }"
  prettyPrint (MissingRecordMember name record) =
    "Cannot find " <> prettyPrint name <> " in { " <> showKeys record <> " }"
  prettyPrint (MissingRecordTypeMember name types) =
    "Cannot find " <> prettyPrint name <> " in { " <> showKeys types <> " }"
  prettyPrint (MissingBuiltIn name) =
    "Cannot find built-in function " <> prettyPrint name
  prettyPrint (CannotMatchRecord env mt) =
    "Cannot match type " <> prettyPrint mt <> " to record in { " <> showKeys env <> " }"
  prettyPrint (CaseMatchExpectedSum mt) =
    "Expected sum type but got " <> prettyPrint mt
  prettyPrint (CaseMatchExpectedPair mt) =
    "Expected pair but got " <> prettyPrint mt
  prettyPrint (CaseMatchExpectedList mt) =
    "Expected list but got " <> prettyPrint mt
  prettyPrint (CaseMatchExpectedLambda l r) =
    "Expected lambdas but got " <> prettyPrint l <> " and " <> prettyPrint r

instance Semigroup TypeError where
  _ <> b = b

instance Monoid TypeError where
  mempty = UnknownTypeError
