{-# LANGUAGE DerivingStrategies #-}

module Smol.Core.Modules.Types.ModuleError
  ( ModuleError (..),
    moduleErrorDiagnostic,
    ResolveDepsError (..),
    TestError (..),
  )
where

import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import Smol.Core.Interpreter.Types.InterpreterError
import Smol.Core.Modules.Types.DefIdentifier
import Smol.Core.Modules.Types.ModuleName
import Smol.Core.Modules.Types.TestName
import Smol.Core.Typecheck
import Smol.Core.Types

data TestError ann
  = TestDoesNotTypecheck Text Identifier (TCError ann)
  deriving stock (Eq, Ord, Show)

testErrorDiagnostic :: TestError Annotation -> Diag.Diagnostic Text
testErrorDiagnostic (TestDoesNotTypecheck input _ typeErr) =
  typeErrorDiagnostic input typeErr

data ResolveDepsError
  = VarNotFound Identifier
  | CannotFindTypes (Set TypeName)
  deriving stock (Eq, Ord, Show)

resolveDepsErrorDiagnostic :: ResolveDepsError -> Diag.Diagnostic Text
resolveDepsErrorDiagnostic (VarNotFound ident) =
  let report =
        Diag.Err
          Nothing
          (T.pack $ "Variable not found: " <> show ident)
          []
          []
   in Diag.addReport Diag.def report
resolveDepsErrorDiagnostic (CannotFindTypes tys) =
  let report =
        Diag.Err
          Nothing
          (T.pack $ "Types not found: " <> show tys)
          []
          []
   in Diag.addReport Diag.def report

data ModuleError ann
  = DuplicateDefinition Identifier
  | DuplicateTypeName TypeName
  | DuplicateConstructor Constructor
  | CannotFindValues (Set Identifier)
  | CannotFindConstructors (Set Constructor)
  | ErrorInResolveDeps ResolveDepsError
  | DefDoesNotTypeCheck Text (DefIdentifier ResolvedDep) (TCError ann)
  | NamedImportNotFound (Set ModuleName) ModuleName
  | EmptyTestName Identifier
  | ErrorInTest TestName (TestError ann)
  | ErrorInInterpreter (InterpreterError ann)
  deriving stock (Eq, Ord, Show)

moduleErrorDiagnostic :: ModuleError Annotation -> Diag.Diagnostic Text
moduleErrorDiagnostic (DefDoesNotTypeCheck input _ typeErr) =
  typeErrorDiagnostic input typeErr
moduleErrorDiagnostic (ErrorInTest _ testErr) =
  testErrorDiagnostic testErr
moduleErrorDiagnostic (ErrorInResolveDeps resolveErr) =
  resolveDepsErrorDiagnostic resolveErr
moduleErrorDiagnostic other =
  let report =
        Diag.Err
          Nothing
          (T.pack (show other))
          []
          []
   in Diag.addReport Diag.def report
