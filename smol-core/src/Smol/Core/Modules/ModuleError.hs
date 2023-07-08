{-# LANGUAGE DerivingStrategies #-}

module Smol.Core.Modules.ModuleError
  ( ModuleError (..),
    moduleErrorDiagnostic,
  )
where

import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import Smol.Core.Typecheck
import Smol.Core.Types
import Smol.Core.Types.Module.DefIdentifier
import Smol.Core.Types.Module.ModuleName

data ModuleError
  = DuplicateDefinition Identifier
  | DuplicateTypeName TypeName
  | DuplicateConstructor Constructor
  | CannotFindValues (Set Identifier)
  | CannotFindTypes (Set TypeName)
  | CannotFindConstructors (Set Constructor)
  | VarNotFound Identifier
  | DefDoesNotTypeCheck Text DefIdentifier (TCError Annotation)
  | NamedImportNotFound (Set ModuleName) ModuleName
  | DefMissingReturnType DefIdentifier
  | DefMissingTypeAnnotation DefIdentifier Identifier
  | EmptyTestName Identifier 
  deriving stock (Eq, Ord, Show)

moduleErrorDiagnostic :: ModuleError -> Diag.Diagnostic Text
moduleErrorDiagnostic (DefDoesNotTypeCheck input _ typeErr) = typeErrorDiagnostic input typeErr
moduleErrorDiagnostic other =
  let report =
        Diag.Err
          Nothing
          (T.pack (show other))
          []
          []
   in Diag.addReport Diag.def report
