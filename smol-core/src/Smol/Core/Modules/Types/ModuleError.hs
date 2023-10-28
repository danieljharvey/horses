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
import Smol.Core.Modules.Types.TestName
import Smol.Core.Typecheck
import Smol.Core.Types

data TestError ann
  = TestDoesNotTypecheck Text (TCError ann)
  deriving stock (Eq, Ord, Show)

testErrorDiagnostic :: TestError Annotation -> Diag.Diagnostic Text
testErrorDiagnostic (TestDoesNotTypecheck input typeErr) =
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
   in Diag.addReport mempty report
resolveDepsErrorDiagnostic (CannotFindTypes tys) =
  let report =
        Diag.Err
          Nothing
          (T.pack $ "Types not found: " <> show tys)
          []
          []
   in Diag.addReport mempty report

data ModuleError ann
  = DuplicateDefinition Identifier
  | DuplicateTypeName TypeName
  | DuplicateConstructor Constructor
  | DuplicateTypeclass TypeclassName
  | MissingTypeclass TypeclassName
  | ErrorInResolveDeps ResolveDepsError
  | DefDoesNotTypeCheck Text (DefIdentifier ResolvedDep) (TCError ann)
  | DictionaryPassingError Text (TCError ann)
  | EmptyTestName (Expr ParseDep ann)
  | ErrorInTest TestName (TestError ann)
  | ErrorInInterpreter (InterpreterError ann)
  deriving stock (Eq, Ord, Show)

moduleErrorDiagnostic :: ModuleError Annotation -> Diag.Diagnostic Text
moduleErrorDiagnostic (DefDoesNotTypeCheck input _ typeErr) =
  typeErrorDiagnostic input typeErr
moduleErrorDiagnostic (DictionaryPassingError input typeErr) =
  typeErrorDiagnostic input typeErr
moduleErrorDiagnostic (ErrorInTest _ testErr) =
  testErrorDiagnostic testErr
moduleErrorDiagnostic (ErrorInInterpreter interpreterErr) =
  interpreterErrorDiagnostic interpreterErr
moduleErrorDiagnostic (ErrorInResolveDeps resolveErr) =
  resolveDepsErrorDiagnostic resolveErr
moduleErrorDiagnostic (EmptyTestName _expr) =
  Diag.addReport mempty $
        Diag.Err
          Nothing
          (T.pack $ "Test name must not be empty!" )
          []
          []

moduleErrorDiagnostic (DuplicateDefinition ident)
 = Diag.addReport mempty $
        Diag.Err
          Nothing
          (T.pack $ "Duplicate definition in module: " <> show ident )
          []
          []
moduleErrorDiagnostic (DuplicateTypeName typeName)
 = Diag.addReport mempty $
        Diag.Err
          Nothing
          (T.pack $ "Duplicate type name definition in module: " <> show typeName )
          []
          []
moduleErrorDiagnostic (DuplicateConstructor constructor)
 = Diag.addReport mempty $
        Diag.Err
          Nothing
          (T.pack $ "Duplicate constructor defined in module: " <> show constructor )
          []
          []
moduleErrorDiagnostic (DuplicateTypeclass typeclassName)
 = Diag.addReport mempty $
        Diag.Err
          Nothing
          (T.pack $ "Duplicate typeclass defined in module: " <> show typeclassName )
          []
          []
moduleErrorDiagnostic (MissingTypeclass typeclassName)
 = Diag.addReport mempty $
        Diag.Err
          Nothing
          (T.pack $ "Could not find typeclass: " <> show typeclassName )
          []
          []






    {-
moduleErrorDiagnostic other =
  let report =
        Diag.Err
          Nothing
          (T.pack (show other))
          []
          []
   in Diag.addReport mempty report

-}
