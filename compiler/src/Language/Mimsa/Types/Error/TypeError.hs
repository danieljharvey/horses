{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.TypeError
  ( TypeErrorF (..),
    TypeError,
    getErrorPos,
    getAllAnnotations,
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
import Language.Mimsa.Types.Error.PatternMatchError (PatternMatchErrorF (..))
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps (Swaps)
import Language.Mimsa.Types.Typechecker.Environment (Environment (getDataTypes))
import Language.Mimsa.Types.Typechecker.MonoType
import Prettyprinter
import Text.Megaparsec

data TypeErrorF ann
  = UnknownTypeError
  | FailsOccursCheck Swaps TypeIdentifier (Type ann)
  | UnificationError (Type ann) (Type ann)
  | VariableNotInEnv Swaps ann Variable (Set TypeIdentifier)
  | MissingRecordMember ann Name (Set Name)
  | MissingRecordTypeMember ann Name (Map Name (Type ann))
  | NoFunctionEquality (Type ann) (Type ann)
  | CannotMatchRecord Environment ann (Type ann)
  | CaseMatchExpectedPair ann (Type ann)
  | TypeConstructorNotInScope Environment ann TyCon
  | TypeVariablesNotInDataType TyCon (Set Name) (Set Name)
  | ConflictingConstructors ann TyCon
  | RecordKeyMismatch (Set Name)
  | DuplicateTypeDeclaration TyCon
  | IncompletePatternMatch ann [TyCon]
  | MixedUpPatterns [TyCon]
  | TypedHoles (Map Name (Type ann, Set Name))
  | CouldNotFindInfixOperator ann InfixOp (Set InfixOp)
  | CannotUseBuiltInTypeAsConstructor ann TyCon
  | InternalConstructorUsedOutsidePatternMatch ann TyCon
  | PatternMatchErr (PatternMatchErrorF ann)
  deriving stock (Eq, Ord, Show, Foldable)

type TypeError = TypeErrorF Annotation

------

instance Semigroup (TypeErrorF ann) where
  a <> _ = a

instance Monoid (TypeErrorF ann) where
  mempty = UnknownTypeError

instance (Printer ann) => Printer (TypeErrorF ann) where
  prettyDoc = vsep . renderTypeError

instance ShowErrorComponent (TypeErrorF Annotation) where
  showErrorComponent = T.unpack . prettyPrint
  errorComponentLen typeErr = let (_, len) = getErrorPos typeErr in len

type Start = Int

type Length = Int

-- megaparsec accepts one single error range
fromAnnotation :: Annotation -> (Start, Length)
fromAnnotation (Location a b) = (a, b - a)
fromAnnotation _ = (0, 0)

-- get overall error position for Megaparsec
getErrorPos :: TypeError -> (Start, Length)
getErrorPos = fromAnnotation . mconcat . getAllAnnotations

-- use the derived Foldable instance to get all annotations in an error
getAllAnnotations :: TypeError -> [Annotation]
getAllAnnotations = foldMap pure

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

renderTypeError :: (Printer ann) => TypeErrorF ann -> [Doc a]
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
