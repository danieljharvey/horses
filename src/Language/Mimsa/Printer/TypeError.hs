{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Printer.TypeError
  ( renderTypeError,
  )
where

import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Prettyprint.Doc
import Language.Mimsa.Printer.Construct
import Language.Mimsa.Printer.MonoType
import Language.Mimsa.Printer.Name
import Language.Mimsa.Printer.TypeName
import Language.Mimsa.Printer.Variable
import Language.Mimsa.Types

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
  [ renderVariable var <+> "appears inside" <+> renderMonoType mt <+> ".",
    "Swaps:"
  ]
    <> showMap renderVariable renderName swaps
renderTypeError (UnificationError a b) =
  [ "Unification error",
    "Cannot match" <+> renderMonoType a <+> "and" <+> renderMonoType b
  ]
renderTypeError (CannotUnifyBoundVariable tv mt) =
  ["Cannot unify type", renderMonoType mt, "with bound variable" <+> renderVariable tv]
renderTypeError (CannotCaseMatchOnType ty) =
  ["Cannot case match on type", pretty (show ty)]
renderTypeError (VariableNotInEnv swaps name members) =
  ["Variable" <+> renderName (withSwap swaps name) <+> " not in scope."]
    <> showSet renderVariable members
renderTypeError (MissingRecordMember name members) =
  [ "Cannot find" <+> renderName name <+> ".",
    "The following are available:"
  ]
    <> showSet renderName members
renderTypeError (MissingRecordTypeMember name types) =
  [ "Cannot find" <+> renderName name <+> ".",
    "The following types are available:"
  ]
    <> showKeys renderName types
renderTypeError (MissingBuiltIn var) =
  ["Cannot find built-in function" <+> renderVariable var]
renderTypeError (CannotMatchRecord env mt) =
  [ "Cannot match type" <+> renderMonoType mt <+> "to record.",
    "The following are available:",
    pretty (show env)
  ]
renderTypeError (CaseMatchExpectedPair mt) =
  ["Expected pair but got" <+> renderMonoType mt]
renderTypeError (TypeConstructorNotInScope env constructor) =
  [ "Type constructor for" <+> renderConstruct constructor
      <+> "not found in scope.",
    "The following are available:"
  ]
    <> printDataTypes env
renderTypeError (TypeIsNotConstructor expr) =
  ["Type" <+> pretty (show expr) <+> "is not a constructor."]
renderTypeError (ConflictingConstructors constructor) =
  ["Multiple constructors found matching" <+> renderConstruct constructor]
renderTypeError (CannotApplyToType constructor) =
  ["Cannot apply value to" <+> renderConstruct constructor]
renderTypeError (DuplicateTypeDeclaration constructor) =
  ["Cannot redeclare existing type name" <+> renderConstruct constructor]
renderTypeError (TypeVariableNotInDataType constructor name as) =
  [ "Type variable" <+> renderName name
      <+> "could not be in found in type vars for"
      <+> renderConstruct constructor,
    "The following type variables were found:"
  ]
    <> (renderName <$> as)
renderTypeError (IncompletePatternMatch names) =
  [ "Incomplete pattern match.",
    "Missing constructors:"
  ]
    <> (renderConstruct <$> names)
renderTypeError (MixedUpPatterns names) =
  [ "Mixed up patterns in same match.",
    "Constructors:"
  ]
    <> (renderConstruct <$> names)

printDataTypes :: Environment -> [Doc ann]
printDataTypes env = mconcat $ snd <$> M.toList (printDt <$> getDataTypes env)
  where
    printDt :: DataType -> [Doc ann]
    printDt (DataType tyName tyVars constructors) =
      [renderConstruct tyName] <> printTyVars tyVars
        <> zipWith (<>) (":" : repeat "|") (printCons <$> M.toList constructors)
    printTyVars as = renderName <$> as
    printCons (consName, args) =
      fold
        ( [ renderConstruct
              consName
          ]
            <> (renderTypeName <$> args)
        )
