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
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.Environment
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Printer
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Variable

data TypeError
  = UnknownTypeError
  | FailsOccursCheck Swaps Variable MonoType
  | UnificationError MonoType MonoType
  | VariableNotInEnv Variable (Set Variable)
  | MissingRecordMember Name (Set Name)
  | MissingRecordTypeMember Name (Map Name MonoType)
  | MissingBuiltIn Variable
  | CannotUnifyBoundVariable Variable MonoType
  | CannotMatchRecord Environment MonoType
  | CaseMatchExpectedSum MonoType
  | CaseMatchExpectedPair MonoType
  | CaseMatchExpectedList MonoType
  | CaseMatchExpectedLambda (Expr Variable) (Expr Variable)
  | TypeConstructorNotInScope Environment Construct
  | TypeIsNotConstructor (Expr Variable)
  | ConflictingConstructors Construct
  | CannotApplyToType Construct
  | DuplicateTypeDeclaration Construct
  deriving (Eq, Ord, Show)

showKeys :: (Printer p) => Map p a -> Text
showKeys record = T.intercalate ", " (prettyPrint <$> M.keys record)

showSet :: (Printer a) => Set a -> Text
showSet set = T.intercalate ", " (prettyPrint <$> S.toList set)

showMap :: (Printer k, Printer a) => Map k a -> Text
showMap map' = T.intercalate ", " (prettyPrint <$> M.toList map')

instance Printer TypeError where
  prettyPrint UnknownTypeError =
    "Unknown type error"
  prettyPrint (FailsOccursCheck swaps name mt) =
    prettyPrint name <> " appears inside " <> prettyPrint mt <> ". Swaps: " <> showMap swaps
  prettyPrint (UnificationError a b) =
    "Unification error - cannot match " <> prettyPrint a <> " and " <> prettyPrint b
  prettyPrint (CannotUnifyBoundVariable tv mt) =
    "Cannot unify type " <> prettyPrint mt <> " with bound variable " <> prettyPrint tv
  prettyPrint (VariableNotInEnv name members) =
    "Variable " <> prettyPrint name <> " not in scope: { " <> showSet members <> " }"
  prettyPrint (MissingRecordMember name members) =
    "Cannot find " <> prettyPrint name <> " in { " <> showSet members <> " }"
  prettyPrint (MissingRecordTypeMember name types) =
    "Cannot find " <> prettyPrint name <> " in { " <> showKeys types <> " }"
  prettyPrint (MissingBuiltIn name) =
    "Cannot find built-in function " <> prettyPrint name
  prettyPrint (CannotMatchRecord env mt) =
    "Cannot match type " <> prettyPrint mt <> " to record in { " <> prettyPrint env <> " }"
  prettyPrint (CaseMatchExpectedSum mt) =
    "Expected sum type but got " <> prettyPrint mt
  prettyPrint (CaseMatchExpectedPair mt) =
    "Expected pair but got " <> prettyPrint mt
  prettyPrint (CaseMatchExpectedList mt) =
    "Expected list but got " <> prettyPrint mt
  prettyPrint (CaseMatchExpectedLambda l r) =
    "Expected lambdas but got " <> prettyPrint l <> " and " <> prettyPrint r
  prettyPrint (TypeConstructorNotInScope env name) =
    "Type constructor for " <> prettyPrint name
      <> " not found in scope: "
      <> printDataTypes env
  prettyPrint (TypeIsNotConstructor a) =
    "Type " <> prettyPrint a <> " is not a constructor"
  prettyPrint (ConflictingConstructors name) =
    "Multiple constructors found matching " <> prettyPrint name
  prettyPrint (CannotApplyToType name) =
    "Cannot apply value to " <> prettyPrint name
  prettyPrint (DuplicateTypeDeclaration name) =
    "Cannot redeclare existing type name " <> prettyPrint name

printDataTypes :: Environment -> Text
printDataTypes env = T.intercalate "\n" (printDt <$> M.toList (getDataTypes env))
  where
    printDt (tyName, constructors) =
      prettyPrint tyName
        <> ": "
        <> T.intercalate " | " (printCons <$> M.toList constructors)
    printCons (consName, args) =
      prettyPrint consName
        <> " "
        <> T.intercalate " " (prettyPrint <$> args)

instance Semigroup TypeError where
  a <> _ = a

instance Monoid TypeError where
  mempty = UnknownTypeError
