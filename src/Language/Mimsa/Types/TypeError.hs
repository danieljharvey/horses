{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.TypeError
  ( TypeError (..),
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.Environment
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Variable

data TypeError
  = UnknownTypeError
  | FailsOccursCheck Swaps Variable MonoType
  | UnificationError MonoType MonoType
  | VariableNotInEnv Swaps Variable (Set Variable)
  | MissingRecordMember Name (Set Name)
  | MissingRecordTypeMember Name (Map Name MonoType)
  | MissingBuiltIn Variable
  | CannotUnifyBoundVariable Variable MonoType
  | CannotMatchRecord Environment MonoType
  | CaseMatchExpectedPair MonoType
  | CaseMatchExpectedLambda (Expr Variable) (Expr Variable)
  | CannotCaseMatchOnType (Expr Variable)
  | TypeConstructorNotInScope Environment Construct
  | TypeIsNotConstructor (Expr Variable)
  | TypeVariableNotInDataType Construct Name [Name]
  | ConflictingConstructors Construct
  | CannotApplyToType Construct
  | DuplicateTypeDeclaration Construct
  | IncompletePatternMatch [Construct]
  | MixedUpPatterns [Construct]
  deriving (Eq, Ord, Show)

showKeys :: (Printer p) => Map p a -> Text
showKeys record = T.intercalate ", " (prettyPrint <$> M.keys record)

showSet :: (Printer a) => Set a -> Text
showSet set = T.intercalate ", " (prettyPrint <$> S.toList set)

showMap :: (Printer k, Printer a) => Map k a -> Text
showMap map' = T.intercalate ", " (prettyPrint <$> M.toList map')

withSwap :: Swaps -> Variable -> Name
withSwap _ (BuiltIn n) = n
withSwap _ (BuiltInActual n _) = n
withSwap _ (NamedVar n) = n
withSwap swaps (NumberedVar i) = fromMaybe (mkName "unknownvar") (M.lookup (NumberedVar i) swaps)

instance Printer TypeError where
  prettyPrint UnknownTypeError =
    "Unknown type error"
  prettyPrint (FailsOccursCheck swaps name mt) =
    prettyPrint name <> " appears inside " <> prettyPrint mt <> ". Swaps: " <> showMap swaps
  prettyPrint (UnificationError a b) =
    "Unification error - cannot match " <> prettyPrint a <> " and " <> prettyPrint b
  prettyPrint (CannotUnifyBoundVariable tv mt) =
    "Cannot unify type " <> prettyPrint mt <> " with bound variable " <> prettyPrint tv
  prettyPrint (CannotCaseMatchOnType ty) = "Cannot case match on type: " <> prettyPrint ty
  prettyPrint (VariableNotInEnv swaps name members) =
    "Variable " <> prettyPrint (withSwap swaps name) <> " not in scope: { " <> showSet members <> " }"
  prettyPrint (MissingRecordMember name members) =
    "Cannot find " <> prettyPrint name <> " in { " <> showSet members <> " }"
  prettyPrint (MissingRecordTypeMember name types) =
    "Cannot find " <> prettyPrint name <> " in { " <> showKeys types <> " }"
  prettyPrint (MissingBuiltIn name) =
    "Cannot find built-in function " <> prettyPrint name
  prettyPrint (CannotMatchRecord env mt) =
    "Cannot match type " <> prettyPrint mt <> " to record in { " <> prettyPrint env <> " }"
  prettyPrint (CaseMatchExpectedPair mt) =
    "Expected pair but got " <> prettyPrint mt
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
  prettyPrint (TypeVariableNotInDataType ty a as) =
    "Type variable " <> prettyPrint a <> " could not be in found in type vars for " <> prettyPrint ty
      <> ". The following type variables were found: ["
      <> T.intercalate ", " (prettyPrint <$> as)
      <> "]"
  prettyPrint (IncompletePatternMatch names) =
    "Incomplete pattern match. Missing constructors: ["
      <> T.intercalate ", " (prettyPrint <$> names)
      <> "]"
  prettyPrint (MixedUpPatterns names) =
    "Mixed up patterns in same match. Constructors: ["
      <> T.intercalate ", " (prettyPrint <$> names)
      <> "]"

printDataTypes :: Environment -> Text
printDataTypes env = T.intercalate "\n" $ snd <$> M.toList (printDt <$> getDataTypes env)
  where
    printDt (DataType tyName tyVars constructors) =
      prettyPrint tyName <> " " <> printTyVars tyVars
        <> ": "
        <> T.intercalate " | " (printCons <$> M.toList constructors)
    printTyVars as = T.intercalate " " (prettyPrint <$> as)
    printCons (consName, args) =
      prettyPrint consName
        <> " "
        <> T.intercalate " " (prettyPrint <$> args)

instance Semigroup TypeError where
  a <> _ = a

instance Monoid TypeError where
  mempty = UnknownTypeError
