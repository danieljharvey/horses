{-# LANGUAGE DerivingStrategies #-}

module Smol.Core.Modules.Types.ModuleError
  ( ModuleError (..),
    moduleErrorDiagnostic,
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

data ModuleError ann
  = DuplicateDefinition Identifier
  | DuplicateTypeName TypeName
  | DuplicateConstructor Constructor
  | CannotFindValues (Set Identifier)
  | CannotFindTypes (Set TypeName)
  | CannotFindConstructors (Set Constructor)
  | VarNotFound Identifier
  | DefDoesNotTypeCheck Text DefIdentifier (TCError ann)
  | NamedImportNotFound (Set ModuleName) ModuleName
  | DefMissingReturnType DefIdentifier
  | DefMissingTypeAnnotation DefIdentifier Identifier
  | EmptyTestName Identifier
  | ErrorInTest TestName (TestError ann)
  | ErrorInInterpreter (InterpreterError ann)
  deriving stock (Eq, Ord, Show)

moduleErrorDiagnostic :: ModuleError Annotation -> Diag.Diagnostic Text
moduleErrorDiagnostic (DefDoesNotTypeCheck input _ typeErr) =
  typeErrorDiagnostic input typeErr
moduleErrorDiagnostic (ErrorInTest _ testErr) =
  testErrorDiagnostic testErr
moduleErrorDiagnostic other =
  let report =
        Diag.Err
          Nothing
          (T.pack (show other))
          []
          []
   in Diag.addReport Diag.def report
