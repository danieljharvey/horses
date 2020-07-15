{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.TypeError
  ( TypeError (..),
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
import Language.Mimsa.Types.Variable

data TypeError
  = UnknownTypeError
  | FailsOccursCheck TypeVar MonoType
  | UnificationError MonoType MonoType
  | VariableNotInEnv Variable (Set Variable)
  | MissingRecordMember Name (Set Name)
  | MissingRecordTypeMember Name (Map Name MonoType)
  | MissingBuiltIn Variable
  | CannotMatchRecord Environment MonoType
  | CaseMatchExpectedSum MonoType
  | CaseMatchExpectedPair MonoType
  | CaseMatchExpectedList MonoType
  | CaseMatchExpectedLambda (Expr Variable) (Expr Variable)
  deriving (Eq, Ord, Show)

showKeys :: (Printer p) => Map p a -> Text
showKeys record = T.intercalate ", " (prettyPrint <$> M.keys record)

showSet :: (Printer a) => Set a -> Text
showSet set = T.intercalate ", " (prettyPrint <$> S.toList set)

instance Printer TypeError where
  prettyPrint UnknownTypeError =
    "Unknown type error"
  prettyPrint (FailsOccursCheck name mt) =
    prettyPrint name <> " appears inside " <> prettyPrint mt
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
