{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Mimsa.Types.Error.TypeError
  ( TypeErrorF (..),
    TypeError,
    getErrorPos,
    getAllAnnotations,
    typeErrorDiagnostic,
  )
where

import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Error.Diagnose
import Language.Mimsa.Printer
import Language.Mimsa.Project.SourceSpan
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.PatternMatchError (PatternMatchErrorF (..))
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project.SourceSpan
import Language.Mimsa.Types.Typechecker.Environment (Environment (getDataTypes))
import Language.Mimsa.Types.Typechecker.FoundPath
import Language.Mimsa.Types.Typechecker.MonoType
import Prettyprinter
import Text.Megaparsec

data TypeErrorF var ann
  = UnknownTypeError
  | FailsOccursCheck TypeIdentifier (Type ann)
  | UnificationError (Type ann) (Type ann)
  | MissingRecordMember ann var (Set var)
  | MissingRecordTypeMember ann var (Map Name (Type ann))
  | NoFunctionEquality (Type ann) (Type ann)
  | CannotMatchRecord Environment ann (Type ann)
  | TypeConstructorNotInScope Environment ann TyCon
  | TypeVariablesNotInDataType TyCon (Set var) (Set var)
  | ConflictingConstructors ann TyCon
  | RecordKeyMismatch (Set Name)
  | DuplicateTypeDeclaration ann TyCon
  | IncompletePatternMatch ann [TyCon]
  | MixedUpPatterns [TyCon]
  | TypedHoles (Map Name (Type ann, Set FoundPath))
  | CouldNotFindInfixOperator ann InfixOp (Set InfixOp)
  | CannotUseBuiltInTypeAsConstructor ann TyCon
  | InternalConstructorUsedOutsidePatternMatch ann TyCon
  | PatternMatchErr (PatternMatchErrorF var ann)
  | NameNotFoundInScope ann (Set var) var
  | VariableNotFound ann (Set TypeIdentifier) var
  deriving stock (Eq, Ord, Show, Foldable)

type TypeError = TypeErrorF Name Annotation

------

instance Semigroup (TypeErrorF var ann) where
  a <> _ = a

instance Monoid (TypeErrorF var ann) where
  mempty = UnknownTypeError

instance
  (Printer ann, Show ann, Printer var, Printer (Pattern var ann)) =>
  Printer (TypeErrorF var ann)
  where
  prettyDoc = vsep . renderTypeError

instance ShowErrorComponent (TypeErrorF Name Annotation) where
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
showKeys renderP record = dquotes . renderP <$> M.keys record

showSet :: (a -> Doc ann) -> Set a -> [Doc ann]
showSet renderA set = renderA <$> S.toList set

showMap :: (k -> Doc ann) -> (a -> Doc ann) -> Map k a -> [Doc ann]
showMap renderK renderA map' =
  (\(k, a) -> renderK k <+> ":" <+> renderA a)
    <$> M.toList map'

------

-----

renderTypeError :: (Printer ann, Printer var, Printer (Pattern var ann)) => TypeErrorF var ann -> [Doc a]
renderTypeError UnknownTypeError =
  ["Unknown type error"]
renderTypeError (FailsOccursCheck var mt) =
  [ prettyDoc var <+> "appears inside" <+> prettyDoc mt <+> "."
  ]
renderTypeError (UnificationError a b) =
  [ "Unification error",
    "Cannot match" <+> prettyDoc a <+> "and" <+> prettyDoc b
  ]
renderTypeError (CouldNotFindInfixOperator _ op allOps) =
  [ "Could not find infix operator " <> prettyDoc op,
    "The following are available:"
  ]
    <> showSet prettyDoc allOps
renderTypeError (MissingRecordMember _ name members) =
  [ "Cannot find" <+> prettyDoc name <> ".",
    "The following are available:"
  ]
    <> showSet prettyDoc members
renderTypeError (MissingRecordTypeMember _ name types) =
  [ "Cannot find" <+> dquotes (prettyDoc name) <> ". The following record items are available:"
  ]
    <> showKeys prettyDoc types
renderTypeError (CannotMatchRecord env _ mt) =
  [ "Cannot match type" <+> prettyDoc mt <+> "to record.",
    "The following are available:",
    pretty (show env)
  ]
renderTypeError (TypeConstructorNotInScope env _ constructor) =
  [ "Type constructor for" <+> prettyDoc constructor
      <+> "not found in scope.",
    "The following are available:"
  ]
    <> printDataTypes env
renderTypeError (ConflictingConstructors _ constructor) =
  ["Multiple constructors found matching" <+> prettyDoc constructor]
renderTypeError (DuplicateTypeDeclaration _ constructor) =
  ["Cannot redeclare existing type name" <+> prettyDoc constructor]
renderTypeError (RecordKeyMismatch keys) =
  [ "Record key mismatch",
    "The following keys were expected to be in both records and were not:"
  ]
    <> showSet prettyDoc keys
renderTypeError (TypeVariablesNotInDataType constructor names as) =
  [ "Type variables" <+> mconcat (showSet prettyDoc names)
      <+> "could not be in found in type vars for"
      <+> prettyDoc constructor,
    "The following type variables were found:"
  ]
    <> showSet prettyDoc as
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
renderTypeError (PatternMatchErr pmErr) =
  [prettyDoc pmErr]
renderTypeError (NameNotFoundInScope _ available name) =
  ["Could not find var " <+> prettyDoc name <+> "in" <+> itemList]
  where
    itemList = "[" <+> pretty (T.intercalate ", " (prettyPrint <$> S.toList available)) <+> "]"
renderTypeError (VariableNotFound _ available name) =
  ["Could not find var " <+> prettyDoc name <+> "in scope" <+> itemList]
  where
    itemList =
      "["
        <+> pretty
          ( T.intercalate
              ", "
              (prettyPrint <$> S.toList available)
          )
        <+> "]"

printDataTypes :: Environment -> [Doc style]
printDataTypes env = mconcat $ snd <$> M.toList (printDt <$> getDataTypes env)
  where
    printDt :: DataType -> [Doc style]
    printDt (DataType tyName tyVars constructors) =
      [prettyDoc tyName] <> printTyVars tyVars
        <> zipWith (<>) (":" : repeat "|") (printCons <$> M.toList constructors)
    printTyVars as = prettyDoc <$> as
    printCons (consName, args) =
      fold
        ( [ prettyDoc
              consName
          ]
            <> (prettyDoc <$> args)
        )

positionFromAnnotation ::
  String ->
  Text ->
  Annotation ->
  Maybe Position
positionFromAnnotation path input ann =
  let toPos ss =
        Position
          (ssRowStart ss, ssColStart ss)
          (ssRowEnd ss, ssColEnd ss)
          path
   in toPos <$> sourceSpan input ann

typeErrorDiagnostic :: Text -> TypeError -> Diagnostic Text
typeErrorDiagnostic input e =
  let filename = "<repl>"
      diag = addFile def filename (T.unpack input)
   in case e of
        (UnificationError a b) ->
          let report =
                err
                  Nothing
                  ( "Unification error! Expected matching types but found "
                      <> prettyPrint a
                      <> " and "
                      <> prettyPrint b
                      <> "."
                  )
                  ( catMaybes
                      [ (,)
                          <$> positionFromAnnotation
                            filename
                            input
                            (getAnnotationForType a)
                          <*> pure
                            ( This ("This has type " <> prettyPrint a)
                            ),
                        (,)
                          <$> positionFromAnnotation
                            filename
                            input
                            (getAnnotationForType b)
                          <*> pure (Where ("This has type " <> prettyPrint b))
                      ]
                  )
                  ["These two values should be of the same type"]
           in addReport diag report
        (MissingRecordTypeMember ann missing _types) ->
          let report =
                err
                  Nothing
                  (prettyPrint e)
                  ( catMaybes
                      [ (,)
                          <$> positionFromAnnotation
                            filename
                            input
                            ann
                          <*> pure
                            ( Where
                                ( "Should contain a member called "
                                    <> prettyPrint missing
                                )
                            )
                      ]
                  )
                  []
           in addReport diag report
        (DuplicateTypeDeclaration ann _constructor) ->
          let report =
                err
                  Nothing
                  (prettyPrint e)
                  ( catMaybes
                      [ (,)
                          <$> positionFromAnnotation
                            filename
                            input
                            ann
                          <*> pure
                            (Where "Is a duplicate")
                      ]
                  )
                  []
           in addReport diag report
        other ->
          let positions =
                mapMaybe
                  (positionFromAnnotation filename input)
                  (getAllAnnotations other)
              report =
                err
                  Nothing
                  (prettyPrint other)
                  ((,Where "") <$> positions)
                  []
           in addReport diag report
