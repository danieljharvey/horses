{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.TypeError
  ( TypeError (..),
    getErrorPos,
  )
where

import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.PatternMatchError (PatternMatchError (..))
import qualified Language.Mimsa.Types.Error.PatternMatchError as Pat
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps (Swaps)
import Language.Mimsa.Types.Typechecker.Environment (Environment (getDataTypes))
import Language.Mimsa.Types.Typechecker.MonoType
import Prettyprinter
import Text.Megaparsec

data TypeError
  = UnknownTypeError
  | FailsOccursCheck Swaps TypeIdentifier MonoType
  | UnificationError MonoType MonoType
  | VariableNotInEnv Swaps Annotation Variable (Set TypeIdentifier)
  | MissingRecordMember Annotation Name (Set Name)
  | MissingRecordTypeMember Annotation Name (Map Name MonoType)
  | NoFunctionEquality MonoType MonoType
  | CannotMatchRecord Environment Annotation MonoType
  | CaseMatchExpectedPair Annotation MonoType
  | TypeConstructorNotInScope Environment Annotation TyCon
  | TypeVariablesNotInDataType TyCon (Set Name) (Set Name)
  | ConflictingConstructors Annotation TyCon
  | RecordKeyMismatch (Set Name)
  | DuplicateTypeDeclaration TyCon
  | IncompletePatternMatch Annotation [TyCon]
  | MixedUpPatterns [TyCon]
  | TypedHoles (Map Name (MonoType, Set Name))
  | FunctionArityMismatch Annotation Int MonoType
  | CouldNotFindInfixOperator Annotation InfixOp (Set InfixOp)
  | CannotUseBuiltInTypeAsConstructor Annotation TyCon
  | InternalConstructorUsedOutsidePatternMatch Annotation TyCon
  | PatternMatchErr PatternMatchError
  deriving stock (Eq, Ord, Show)

------

instance Semigroup TypeError where
  a <> _ = a

instance Monoid TypeError where
  mempty = UnknownTypeError

instance Printer TypeError where
  prettyDoc = vsep . renderTypeError

instance ShowErrorComponent TypeError where
  showErrorComponent = T.unpack . prettyPrint
  errorComponentLen typeErr = let (_, len) = getErrorPos typeErr in len

type Start = Int

type Length = Int

fromAnnotation :: Annotation -> (Start, Length)
fromAnnotation (Location a b) = (a, b - a)
fromAnnotation _ = (0, 0)

getErrorPos :: TypeError -> (Start, Length)
getErrorPos (UnificationError a b) =
  fromAnnotation (getAnnotationForType a <> getAnnotationForType b)
getErrorPos (MissingRecordMember ann _ _) = fromAnnotation ann
getErrorPos (MissingRecordTypeMember ann _ _) = fromAnnotation ann
getErrorPos (VariableNotInEnv _ ann _ _) = fromAnnotation ann
getErrorPos (TypeConstructorNotInScope _ ann _) = fromAnnotation ann
getErrorPos (ConflictingConstructors ann _) = fromAnnotation ann
getErrorPos (IncompletePatternMatch ann _) = fromAnnotation ann
getErrorPos (CaseMatchExpectedPair ann _) =
  fromAnnotation ann
getErrorPos (CannotMatchRecord _ ann _) = fromAnnotation ann
getErrorPos (TypedHoles holes) = case M.toList holes of
  ((_, (mt, _)) : _) -> fromAnnotation (getAnnotationForType mt)
  _ -> fromAnnotation mempty
getErrorPos (FunctionArityMismatch ann _ _) = fromAnnotation ann
getErrorPos (CannotUseBuiltInTypeAsConstructor ann _) = fromAnnotation ann
getErrorPos (InternalConstructorUsedOutsidePatternMatch ann _) = fromAnnotation ann
getErrorPos (PatternMatchErr pat) = Pat.getErrorPos pat
getErrorPos _ = (0, 0)

------

showKeys :: (p -> Doc ann) -> Map p a -> [Doc ann]
showKeys renderP record = renderP <$> M.keys record

showSet :: (a -> Doc ann) -> Set a -> [Doc ann]
showSet renderA set = renderA <$> S.toList set

showMap :: (k -> Doc ann) -> (a -> Doc ann) -> Map k a -> [Doc ann]
showMap renderK renderA map' =
  (\(k, a) -> renderK k <+> ":" <+> renderA a)
    <$> M.toList map'

------

withSwap :: Swaps -> Variable -> Name
withSwap _ (NamedVar n) = n
withSwap swaps (NumberedVar i) =
  fromMaybe
    "unknownvar"
    (M.lookup (NumberedVar i) swaps)

-----

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
renderTypeError (CouldNotFindInfixOperator _ op allOps) =
  [ "Could not find infix operator " <> prettyDoc op,
    "The following are available:"
  ]
    <> showSet prettyDoc allOps
renderTypeError (VariableNotInEnv swaps _ name members) =
  ["Variable" <+> renderName (withSwap swaps name) <+> " not in scope."]
    <> showSet prettyDoc members
renderTypeError (MissingRecordMember _ name members) =
  [ "Cannot find" <+> prettyDoc name <> ".",
    "The following are available:"
  ]
    <> showSet renderName members
renderTypeError (MissingRecordTypeMember _ name types) =
  [ "Cannot find" <+> dquotes (renderName name) <> ".",
    "The following record items are available:"
  ]
    <> showKeys renderName types
renderTypeError (CannotMatchRecord env _ mt) =
  [ "Cannot match type" <+> prettyDoc mt <+> "to record.",
    "The following are available:",
    pretty (show env)
  ]
renderTypeError (CaseMatchExpectedPair _ mt) =
  ["Expected pair but got" <+> prettyDoc mt]
renderTypeError (TypeConstructorNotInScope env _ constructor) =
  [ "Type constructor for" <+> prettyDoc constructor
      <+> "not found in scope.",
    "The following are available:"
  ]
    <> printDataTypes env
renderTypeError (ConflictingConstructors _ constructor) =
  ["Multiple constructors found matching" <+> prettyDoc constructor]
renderTypeError (DuplicateTypeDeclaration constructor) =
  ["Cannot redeclare existing type name" <+> prettyDoc constructor]
renderTypeError (RecordKeyMismatch keys) =
  [ "Record key mismatch",
    "The following keys were expected to be in both records and were not:"
  ]
    <> showSet renderName keys
renderTypeError (TypeVariablesNotInDataType constructor names as) =
  [ "Type variables" <+> mconcat (showSet prettyDoc names)
      <+> "could not be in found in type vars for"
      <+> prettyDoc constructor,
    "The following type variables were found:"
  ]
    <> showSet renderName as
renderTypeError (IncompletePatternMatch _ names) =
  [ "Incomplete pattern match.",
    "Missing constructors:"
  ]
    <> (prettyDoc <$> names)
renderTypeError (MixedUpPatterns names) =
  [ "Mixed up patterns in same match.",
    "Constructors:"
  ]
    <> (prettyDoc <$> names)
renderTypeError (NoFunctionEquality a b) =
  ["Cannot use == on functions", prettyDoc a, prettyDoc b]
renderTypeError (TypedHoles map') =
  ["Typed holes found:"] <> showMap renderHoleName renderSuggestion map'
  where
    renderHoleName n = "?" <> prettyDoc n
    renderSuggestion (mt, suggestions) =
      prettyDoc mt <+> renderMatches suggestions
    renderMatches s =
      if S.null s
        then ""
        else line <> indent 2 ("Suggestions:" <+> list (prettyDoc <$> S.toList s))
renderTypeError (FunctionArityMismatch _ i mt) =
  ["Function arity mismatch. Expected " <> pretty i <> " but got " <> prettyDoc mt]
renderTypeError (CannotUseBuiltInTypeAsConstructor _ name) =
  ["Cannot use built-in type as constructor name:" <+> prettyDoc name]
renderTypeError (InternalConstructorUsedOutsidePatternMatch _ tyCon) =
  ["Internal type constructor" <+> prettyDoc tyCon <+> "cannot be used outside of a pattern match"]
renderTypeError (PatternMatchErr pmErr) = [prettyDoc pmErr]

printDataTypes :: Environment -> [Doc style]
printDataTypes env = mconcat $ snd <$> M.toList (printDt <$> getDataTypes env)
  where
    printDt :: DataType -> [Doc style]
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
