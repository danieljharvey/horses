{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.TypeError
  ( TypeError (..),
    SwappedName (..),
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Printer
import Language.Mimsa.Types.Typechecker

newtype SwappedName = SwappedName Name
  deriving (Eq, Ord, Show)

instance Printer SwappedName where
  prettyPrint (SwappedName a) =
    "'" <> prettyPrint a <> "'"

data TypeError
  = UnknownTypeError
  | FailsOccursCheck SwappedName
  | UnificationError MonoType MonoType
  | VariableNotInEnv SwappedName (Set SwappedName)
  | MissingRecordMember SwappedName (Set SwappedName)
  | MissingRecordTypeMember SwappedName (Map Name MonoType)
  | MissingBuiltIn SwappedName
  | CannotMatchRecord Environment MonoType
  | CaseMatchExpectedSum MonoType
  | CaseMatchExpectedPair MonoType
  | CaseMatchExpectedList MonoType
  | CaseMatchExpectedLambda (Expr Name) (Expr Name)
  deriving (Eq, Ord, Show)

showKeys :: Map Name a -> Text
showKeys record = T.intercalate ", " (prettyPrint <$> M.keys record)

showSet :: (Printer a) => Set a -> Text
showSet set = T.intercalate ", " (prettyPrint <$> S.toList set)

instance Printer TypeError where
  prettyPrint UnknownTypeError =
    "Unknown type error"
  prettyPrint (FailsOccursCheck name) =
    prettyPrint name <> " fails occurs check"
  prettyPrint (UnificationError a b) =
    "Unification error - cannot match " <> prettyPrint a <> " and " <> prettyPrint b
  prettyPrint (VariableNotInEnv name members) =
    "Variable " <> prettyPrint name <> " not in scope: { " <> showSet members <> " }"
  prettyPrint (MissingRecordMember name members) =
    "Cannot find " <> prettyPrint name <> " in { " <> showSet members <> " }"
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
  a <> _ = a

instance Monoid TypeError where
  mempty = UnknownTypeError
