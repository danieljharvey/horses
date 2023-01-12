{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Mimsa.Typechecker.Error.TypeError
  ( TypeErrorF (..),
    TypeError,
    getErrorPos,
    getAllAnnotations,
    typeErrorDiagnostic,
  )
where

import Data.Foldable (fold)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import GHC.Natural
import Language.Mimsa.Core
import Language.Mimsa.Typechecker.Error.PatternMatchError (PatternMatchErrorF (..))
import Language.Mimsa.Typechecker.Types.Environment (Environment (getDataTypes))
import Language.Mimsa.Typechecker.Types.FoundPath
import Prettyprinter
import Text.Megaparsec

data TypeErrorF var ann
  = UnknownTypeError
  | FailsOccursCheck TypeIdentifier (Type ann)
  | UnificationError (Type ann) (Type ann)
  | MissingRecordMember ann var (Set var)
  | MissingRecordTypeMember ann var (Map Name (Type ann))
  | MissingTupleTypeMember ann Natural [Type ann]
  | NoFunctionEquality (Type ann) (Type ann)
  | CannotMatchRecord Environment ann (Type ann)
  | CannotMatchTuple Environment ann (Type ann)
  | TypeConstructorNotInScope Environment ann (Maybe ModuleName) TyCon
  | TypeVariablesNotInDataType ann TypeName (Set var) (Set var)
  | ConflictingConstructors ann TyCon
  | RecordKeyMismatch (Set Name)
  | DuplicateTypeDeclaration ann TypeName
  | IncompletePatternMatch ann [TyCon]
  | MixedUpPatterns [TyCon]
  | TypedHoles (Map Name (Type ann, Set FoundPath))
  | CouldNotFindInfixOperator ann InfixOp (Set InfixOp)
  | CannotUseBuiltInTypeAsConstructor ann TyCon
  | InternalConstructorUsedOutsidePatternMatch ann TyCon
  | PatternMatchErr (PatternMatchErrorF var ann)
  | NameNotFoundInScope ann (Set (var, Maybe ModuleName)) (Maybe ModuleName) var
  | VariableNotFound ann (Set TypeIdentifier) var
  | IfPredicateIsNotBoolean ann (Type ann)
  | FunctionArgumentMismatch ann (Type ann) (Type ann)
  | ApplicationToNonFunction ann (Type ann)
  | UnscopedTypeVarFound ann TypeIdentifier
  | KindMismatchInDataDeclaration ann (Maybe ModuleName) TypeName Int Int
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
renderTypeError (IfPredicateIsNotBoolean _ mt) =
  ["Predicate for an if expression should be a boolean. This has type" <+> prettyDoc mt]
renderTypeError (FunctionArgumentMismatch _ expected actual) =
  [ "Incorrect function argument. Expected "
      <> prettyDoc expected
      <> ", got "
      <> prettyDoc actual
  ]
renderTypeError (ApplicationToNonFunction _ mt) =
  ["Cannot apply to non-function. Expected Function, got " <> prettyDoc mt]
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
renderTypeError (MissingTupleTypeMember _ index types) =
  [ "Cannot find index " <+> dquotes (prettyDoc index) <> ". The following items are available:"
  ]
    <> (prettyDoc <$> types)
renderTypeError (CannotMatchRecord env _ mt) =
  [ "Cannot match type" <+> prettyDoc mt <+> "to record.",
    "The following vars are available:",
    pretty (show env)
  ]
renderTypeError (CannotMatchTuple env _ mt) =
  [ "Cannot match type" <+> prettyDoc mt <+> "to tuple.",
    "The following vars are available:",
    pretty (show env)
  ]
renderTypeError (TypeConstructorNotInScope env _ modName constructor) =
  let prettyName = case modName of
        Just mod' -> prettyDoc mod' <> "." <> prettyDoc constructor
        _ -> prettyDoc constructor
   in [ "Type constructor for"
          <+> prettyName
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
renderTypeError (TypeVariablesNotInDataType _ann constructor names as) =
  [ "Type variables"
      <+> mconcat (showSet prettyDoc names)
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
renderTypeError (NameNotFoundInScope _ available mModName name) =
  case mModName of
    Just modName ->
      ["Could not find" <+> prettyDoc modName <> "." <> prettyDoc name <+> itemList]
    Nothing ->
      ["Could not find " <+> prettyDoc name <+> "in" <+> itemList]
  where
    itemList = "[" <+> pretty (T.intercalate ", " (prettyPrint <$> S.toList available)) <+> "]"
renderTypeError (VariableNotFound _ available name) =
  ["Could not find var" <+> prettyDoc name <+> "in scope" <+> itemList]
  where
    itemList =
      "["
        <+> pretty
          ( T.intercalate
              ", "
              (prettyPrint <$> S.toList available)
          )
        <+> "]"
renderTypeError (UnscopedTypeVarFound _ typeVar) =
  [ "Unscoped type var found: " <> prettyDoc typeVar,
    "This is an implementation error, please complain on Github or write a mean tweet"
  ]
renderTypeError (KindMismatchInDataDeclaration _ modName typeName expectedArgs actualArgs) =
  let nameo = case modName of
        Just m -> prettyDoc m <> "." <> prettyDoc typeName
        _ -> prettyDoc typeName
   in ["Kind mismatch " <> nameo <> " expected " <> pretty expectedArgs <> " but got " <> pretty actualArgs]

printDataTypes :: Environment -> [Doc style]
printDataTypes env = mconcat $ snd <$> M.toList (printDt <$> getDataTypes env)
  where
    printDt :: DataType -> [Doc style]
    printDt (DataType tyName tyVars constructors) =
      [prettyDoc tyName]
        <> printTyVars tyVars
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
  Maybe Diag.Position
positionFromAnnotation path input ann =
  let toPos ss =
        Diag.Position
          (ssRowStart ss, ssColStart ss)
          (ssRowEnd ss, ssColEnd ss)
          path
   in toPos <$> sourceSpan input ann

typeErrorDiagnostic :: Text -> TypeError -> Diag.Diagnostic Text
typeErrorDiagnostic input e =
  let filename = "<repl>"
      diag = Diag.addFile Diag.def filename (T.unpack input)
   in case e of
        (UnificationError a b) ->
          let report =
                Diag.Err
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
                            ( Diag.This ("This has type " <> prettyPrint a)
                            ),
                        (,)
                          <$> positionFromAnnotation
                            filename
                            input
                            (getAnnotationForType b)
                          <*> pure (Diag.Where ("This has type " <> prettyPrint b))
                      ]
                  )
                  ["These two values should be of the same type"]
           in Diag.addReport diag report
        (IfPredicateIsNotBoolean ann mt) ->
          let report =
                Diag.Err
                  Nothing
                  ("Predicate for an if expression should be a Boolean. This has type " <> prettyPrint mt)
                  ( catMaybes
                      [ (,)
                          <$> positionFromAnnotation
                            filename
                            input
                            (getAnnotationForType mt)
                          <*> pure
                            ( Diag.This ("This has type " <> prettyPrint mt <> " but should have type Boolean")
                            ),
                        (,)
                          <$> positionFromAnnotation
                            filename
                            input
                            ann
                          <*> pure (Diag.Where "error in this expression")
                      ]
                  )
                  ["Change the predicate to be a Boolean type (True or False)"]
           in Diag.addReport diag report
        (MissingRecordTypeMember ann missing _types) ->
          let report =
                Diag.Err
                  Nothing
                  (prettyPrint e)
                  ( catMaybes
                      [ (,)
                          <$> positionFromAnnotation
                            filename
                            input
                            ann
                          <*> pure
                            ( Diag.Where
                                ( "Should contain a member called "
                                    <> prettyPrint missing
                                )
                            )
                      ]
                  )
                  []
           in Diag.addReport diag report
        (DuplicateTypeDeclaration ann _constructor) ->
          let report =
                Diag.Err
                  Nothing
                  (prettyPrint e)
                  ( catMaybes
                      [ (,)
                          <$> positionFromAnnotation
                            filename
                            input
                            ann
                          <*> pure
                            (Diag.Where "Is a duplicate")
                      ]
                  )
                  []
           in Diag.addReport diag report
        (FunctionArgumentMismatch fnAnn expected actual) ->
          let report =
                Diag.Err
                  Nothing
                  ( "Function called with incorrect argument type. Expected "
                      <> prettyPrint expected
                      <> " but got "
                      <> prettyPrint actual
                      <> "."
                  )
                  ( catMaybes
                      [ (,)
                          <$> positionFromAnnotation
                            filename
                            input
                            (getAnnotationForType actual)
                          <*> pure
                            ( Diag.This ("Passed a " <> prettyPrint actual)
                            ),
                        (,)
                          <$> positionFromAnnotation
                            filename
                            input
                            fnAnn
                          <*> pure (Diag.Where $ "Expects a " <> prettyPrint expected)
                      ]
                  )
                  [Diag.Note ("Pass a value of type " <> prettyPrint expected <> " to the function")]
           in Diag.addReport diag report
        (ApplicationToNonFunction _ mt) ->
          let report =
                Diag.Err
                  Nothing
                  ("Cannot apply value to non-function. Expected a function, got " <> prettyPrint mt)
                  ( catMaybes
                      [ (,)
                          <$> positionFromAnnotation
                            filename
                            input
                            (getAnnotationForType mt)
                          <*> pure
                            ( Diag.This ("This should be a function, but instead has type " <> prettyPrint mt)
                            )
                      ]
                  )
                  []
           in Diag.addReport diag report
        other ->
          let positions =
                mapMaybe
                  (positionFromAnnotation filename input)
                  (getAllAnnotations other)
              report =
                Diag.Err
                  Nothing
                  (prettyPrint other)
                  ((,Diag.Where "") <$> positions)
                  []
           in Diag.addReport diag report
