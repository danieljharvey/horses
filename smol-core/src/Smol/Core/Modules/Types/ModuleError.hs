{-# LANGUAGE DerivingStrategies #-}

module Smol.Core.Modules.Types.ModuleError
  ( ModuleError (..),
    moduleErrorDiagnostic,
    ResolveDepsError (..),
    TestError (..),
    Duplicate(..)
  )
where

import Data.Set (Set)
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import Smol.Core.Interpreter.Types.InterpreterError
import Smol.Core.Modules.Types.DefIdentifier
import Smol.Core.Modules.Types.TestName
import Smol.Core.Typecheck
import Smol.Core.Types

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
  deriving (Eq,Ord,Show)

data ModuleError ann
  = DuplicateDefinition (Duplicate Identifier ann)
  | DuplicateTypeName (Duplicate TypeName ann)
  | DuplicateConstructor (Duplicate Constructor ann)
  | DuplicateTypeclass (Duplicate TypeclassName ann)
  | MissingTypeclass TypeclassName
  | ErrorInResolveDeps ResolveDepsError
  | DefDoesNotTypeCheck (DefIdentifier ResolvedDep) (TCError ann)
  | DictionaryPassingError (TCError ann)
  | EmptyTestName (Expr ParseDep ann)
  | ErrorInTest TestName (TestError ann)
  | ErrorInInterpreter (InterpreterError ann)
  deriving stock (Eq, Ord, Show)

moduleErrorDiagnostic :: T.Text -> ModuleError Annotation -> Diag.Diagnostic T.Text
moduleErrorDiagnostic input (DefDoesNotTypeCheck _ typeErr) =
  typeErrorDiagnostic input typeErr
moduleErrorDiagnostic input (DictionaryPassingError typeErr) =
  typeErrorDiagnostic input typeErr
moduleErrorDiagnostic _ (ErrorInTest _ testErr) =
  testErrorDiagnostic testErr
moduleErrorDiagnostic _ (ErrorInInterpreter interpreterErr) =
  interpreterErrorDiagnostic interpreterErr
moduleErrorDiagnostic _ (ErrorInResolveDeps resolveErr) =
  resolveDepsErrorDiagnostic resolveErr
moduleErrorDiagnostic _ (EmptyTestName _expr) =
  Diag.addReport mempty $
        Diag.Err
          Nothing
          (T.pack $ "Test name must not be empty!" )
          []
          []

moduleErrorDiagnostic _ (DuplicateDefinition ident)
 = Diag.addReport mempty $
        Diag.Err
          Nothing
          (T.pack $ "Duplicate definition in module: " <> show ident )
          []
          []
moduleErrorDiagnostic _ (DuplicateTypeName typeName)
 = Diag.addReport mempty $
        Diag.Err
          Nothing
          (T.pack $ "Duplicate type name definition in module: " <> show typeName )
          []
          []
moduleErrorDiagnostic _ (DuplicateConstructor constructor)
 = Diag.addReport mempty $
        Diag.Err
          Nothing
          (T.pack $ "Duplicate constructor defined in module: " <> show constructor )
          []
          []
moduleErrorDiagnostic _ (DuplicateTypeclass typeclassName)
 = Diag.addReport mempty $
        Diag.Err
          Nothing
          (T.pack $ "Duplicate typeclass defined in module: " <> show typeclassName )
          []
          []
moduleErrorDiagnostic _ (MissingTypeclass typeclassName)
 = Diag.addReport mempty $
        Diag.Err
          Nothing
          (T.pack $ "Could not find typeclass: " <> show typeclassName )
          []
          []





