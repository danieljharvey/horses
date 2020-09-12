{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.TypeError
  ( TypeError (..),
  )
where

import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Prettyprint.Doc
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Environment
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Swaps

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

instance Semigroup TypeError where
  a <> _ = a

instance Monoid TypeError where
  mempty = UnknownTypeError

instance Printer TypeError where
  prettyDoc = vsep . renderTypeError

showKeys :: (p -> Doc ann) -> Map p a -> [Doc ann]
showKeys renderP record = renderP <$> M.keys record

showSet :: (a -> Doc ann) -> Set a -> [Doc ann]
showSet renderA set = renderA <$> S.toList set

showMap :: (k -> Doc ann) -> (a -> Doc ann) -> Map k a -> [Doc ann]
showMap renderK renderA map' =
  (\(k, a) -> renderK k <+> ":" <+> renderA a)
    <$> M.toList map'

withSwap :: Swaps -> Variable -> Name
withSwap _ (BuiltIn n) = n
withSwap _ (BuiltInActual n _) = n
withSwap _ (NamedVar n) = n
withSwap swaps (NumberedVar i) =
  fromMaybe
    (mkName "unknownvar")
    (M.lookup (NumberedVar i) swaps)

renderTypeError :: TypeError -> [Doc ann]
renderTypeError UnknownTypeError =
  ["Unknown type error"]
renderTypeError (FailsOccursCheck swaps var mt) =
  [ prettyDoc var <+> "appears inside" <+> prettyDoc mt <+> ".",
    "Swaps:"
  ]
    <> showMap prettyDoc prettyDoc swaps
renderTypeError (UnificationError a b) =
  [ "Unification error",
    "Cannot match" <+> prettyDoc a <+> "and" <+> prettyDoc b
  ]
renderTypeError (CannotUnifyBoundVariable tv mt) =
  ["Cannot unify type", prettyDoc mt, "with bound variable" <+> prettyDoc tv]
renderTypeError (CannotCaseMatchOnType ty) =
  ["Cannot case match on type", pretty (show ty)]
renderTypeError (VariableNotInEnv swaps name members) =
  ["Variable" <+> renderName (withSwap swaps name) <+> " not in scope."]
    <> showSet prettyDoc members
renderTypeError (MissingRecordMember name members) =
  [ "Cannot find" <+> prettyDoc name <+> ".",
    "The following are available:"
  ]
    <> showSet renderName members
renderTypeError (MissingRecordTypeMember name types) =
  [ "Cannot find" <+> renderName name <+> ".",
    "The following types are available:"
  ]
    <> showKeys renderName types
renderTypeError (MissingBuiltIn var) =
  ["Cannot find built-in function" <+> prettyDoc var]
renderTypeError (CannotMatchRecord env mt) =
  [ "Cannot match type" <+> prettyDoc mt <+> "to record.",
    "The following are available:",
    pretty (show env)
  ]
renderTypeError (CaseMatchExpectedPair mt) =
  ["Expected pair but got" <+> prettyDoc mt]
renderTypeError (TypeConstructorNotInScope env constructor) =
  [ "Type constructor for" <+> prettyDoc constructor
      <+> "not found in scope.",
    "The following are available:"
  ]
    <> printDataTypes env
renderTypeError (TypeIsNotConstructor expr) =
  ["Type" <+> pretty (show expr) <+> "is not a constructor."]
renderTypeError (ConflictingConstructors constructor) =
  ["Multiple constructors found matching" <+> prettyDoc constructor]
renderTypeError (CannotApplyToType constructor) =
  ["Cannot apply value to" <+> prettyDoc constructor]
renderTypeError (DuplicateTypeDeclaration constructor) =
  ["Cannot redeclare existing type name" <+> prettyDoc constructor]
renderTypeError (TypeVariableNotInDataType constructor name as) =
  [ "Type variable" <+> renderName name
      <+> "could not be in found in type vars for"
      <+> prettyDoc constructor,
    "The following type variables were found:"
  ]
    <> (renderName <$> as)
renderTypeError (IncompletePatternMatch names) =
  [ "Incomplete pattern match.",
    "Missing constructors:"
  ]
    <> (prettyDoc <$> names)
renderTypeError (MixedUpPatterns names) =
  [ "Mixed up patterns in same match.",
    "Constructors:"
  ]
    <> (prettyDoc <$> names)

printDataTypes :: Environment -> [Doc ann]
printDataTypes env = mconcat $ snd <$> M.toList (printDt <$> getDataTypes env)
  where
    printDt :: DataType -> [Doc ann]
    printDt (DataType tyName tyVars constructors) =
      [prettyDoc tyName] <> printTyVars tyVars
        <> zipWith (<>) (":" : repeat "|") (printCons <$> M.toList constructors)
    printTyVars as = renderName <$> as
    printCons (consName, args) =
      fold
        ( [ prettyDoc
              consName
          ]
            <> (prettyDoc <$> args)
        )
