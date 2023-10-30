{-# LANGUAGE DerivingStrategies #-}

module Smol.Core.Modules.Types.ModuleError
  ( ModuleError (..),
    moduleErrorDiagnostic,
    ResolveDepsError (..),
    TestError (..),
    Duplicate (..),
  )
where

import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import Smol.Core.Interpreter.Types.InterpreterError
import Smol.Core.Modules.Types.DefIdentifier
import Smol.Core.Modules.Types.TestName
import Smol.Core.SourceSpan (sourceSpan, ssColEnd, ssColStart, ssRowEnd, ssRowStart)
import Smol.Core.Typecheck
import Smol.Core.Types

-- todo: make share library for this
positionFromAnnotation ::
  String ->
  T.Text ->
  Annotation ->
  Maybe Diag.Position
positionFromAnnotation path input (Location locStart locEnd) =
  let toPos ss =
        Diag.Position
          (ssRowStart ss, ssColStart ss)
          (ssRowEnd ss, ssColEnd ss)
          path
      dropOneAnn = Location locStart (locEnd - 1) -- TODO: the real fix is drop trailing linebreaks in parsing
   in toPos <$> sourceSpan input dropOneAnn

data TestError ann
  = TestDoesNotTypecheck T.Text (TCError ann)
  deriving stock (Eq, Ord, Show)

testErrorDiagnostic :: TestError Annotation -> Diag.Diagnostic T.Text
testErrorDiagnostic (TestDoesNotTypecheck input typeErr) =
  typeErrorDiagnostic input typeErr

data ResolveDepsError
  = VarNotFound Identifier
  | CannotFindTypes (Set TypeName)
  deriving stock (Eq, Ord, Show)

resolveDepsErrorDiagnostic :: ResolveDepsError -> Diag.Diagnostic T.Text
resolveDepsErrorDiagnostic (VarNotFound ident) =
  let report =
        Diag.Err
          Nothing
          (T.pack $ "Variable not found: " <> show ident)
          []
          []
   in Diag.addReport mempty report
resolveDepsErrorDiagnostic (CannotFindTypes tys) =
  let report =
        Diag.Err
          Nothing
          (T.pack $ "Types not found: " <> show tys)
          []
          []
   in Diag.addReport mempty report

data Duplicate thing ann
  = Duplicate thing ann ann
  deriving stock (Eq, Ord, Show)

data ModuleError ann
  = DuplicateDefinition (Duplicate Identifier ann)
  | DuplicateTypeDefinition (Duplicate Identifier ann)
  | DuplicateTypeName (Duplicate TypeName ann)
  | DuplicateConstructor (Duplicate Constructor ann)
  | DuplicateTypeclass TypeclassName
  | MissingTypeclass ann TypeclassName
  | ErrorInResolveDeps ResolveDepsError
  | DefDoesNotTypeCheck (DefIdentifier ResolvedDep) (TCError ann)
  | DictionaryPassingError (TCError ann)
  | EmptyTestName (Expr ParseDep ann)
  | ErrorInTest TestName (TestError ann)
  | ErrorInInterpreter (InterpreterError ann)
  deriving stock (Eq, Ord, Show)

fromDuplicate :: String -> T.Text -> Duplicate thing Annotation -> [(Diag.Position, Diag.Marker T.Text)]
fromDuplicate filename input (Duplicate _ ann1 ann2) =
  catMaybes
    [ (,)
        <$> positionFromAnnotation
          filename
          input
          ann1
        <*> pure
          ( Diag.This (T.pack "Defined here")
          ),
      (,)
        <$> positionFromAnnotation
          filename
          input
          ann2
        <*> pure (Diag.Where (T.pack "Also defined here"))
    ]

moduleErrorDiagnostic :: T.Text -> ModuleError Annotation -> Diag.Diagnostic T.Text
moduleErrorDiagnostic input moduleError =
  let filename = "<repl>"
      diag = Diag.addFile mempty filename (T.unpack input)
   in case moduleError of
        (DefDoesNotTypeCheck _ typeErr) ->
          typeErrorDiagnostic input typeErr
        (DictionaryPassingError typeErr) ->
          typeErrorDiagnostic input typeErr
        (ErrorInTest _ testErr) ->
          testErrorDiagnostic testErr
        (ErrorInInterpreter interpreterErr) ->
          interpreterErrorDiagnostic interpreterErr
        (ErrorInResolveDeps resolveErr) ->
          resolveDepsErrorDiagnostic resolveErr
        (EmptyTestName _expr) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              (T.pack "Test name must not be empty!")
              []
              []
        (DuplicateDefinition dup@(Duplicate ident _ _)) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              (T.pack $ "Duplicate definition in module: " <> show ident)
              (fromDuplicate filename input dup)
              [Diag.Note $ T.pack "Remove one of these definitions"]
        (DuplicateTypeDefinition dup@(Duplicate ident _ _)) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              (T.pack $ "Duplicate type definition in module: " <> show ident)
              (fromDuplicate filename input dup)
              [Diag.Note $ T.pack "Remove one of these type definitions"]
        (DuplicateTypeName dup@(Duplicate typeName _ _)) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              (T.pack $ "Duplicate type name definition in module: " <> show typeName)
              (fromDuplicate filename input dup)
              [Diag.Note $ T.pack "Remove one of these data type definitions"]
        (DuplicateConstructor dup@(Duplicate constructor _ _)) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              (T.pack $ "Duplicate constructor defined in module: " <> show constructor)
              (fromDuplicate filename input dup)
              [Diag.Note $ T.pack "Remove one of these constructors"]
        (DuplicateTypeclass typeclassName) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              (T.pack $ "Duplicate typeclass defined in module: " <> show typeclassName)
              []
              []
        (MissingTypeclass ann typeclassName) ->
          Diag.addReport diag $
            Diag.Err
              Nothing
              (T.pack $ "Could not find typeclass: " <> show typeclassName)
              ( catMaybes
                  [ (,)
                      <$> positionFromAnnotation
                        filename
                        input
                        ann
                      <*> pure
                        ( Diag.Where (T.pack "Instance defined here")
                        )
                  ]
              )
              []
