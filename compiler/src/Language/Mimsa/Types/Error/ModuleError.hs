{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.ModuleError (ModuleError (..), moduleErrorDiagnostic) where

import Data.Set (Set)
import Data.Text (Text)
import Error.Diagnose
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.TypeError
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules

data ModuleError
  = DuplicateDefinition DefIdentifier
  | DuplicateTypeName TypeName
  | DuplicateConstructor TyCon
  | DefinitionConflictsWithImport DefIdentifier ModuleHash
  | TypeConflictsWithImport TypeName ModuleHash
  | CannotFindValues (Set DefIdentifier)
  | CannotFindConstructor TyCon
  | CannotFindTypes (Set (Maybe ModuleName, TypeName))
  | DefDoesNotTypeCheck Text DefIdentifier TypeError
  | MissingModule ModuleHash
  | MissingModuleDep DefIdentifier ModuleHash
  | MissingModuleTypeDep TypeName ModuleHash
  | DefMissingReturnType DefIdentifier
  | DefMissingTypeAnnotation DefIdentifier Name
  | EmptyTestName (Expr Name ())
  deriving stock (Eq, Ord, Show)

instance Printer ModuleError where
  prettyPrint (DuplicateDefinition name) =
    "Duplicate definition: " <> prettyPrint name
  prettyPrint (DuplicateTypeName tyName) =
    "Duplicate type name: " <> prettyPrint tyName
  prettyPrint (DuplicateConstructor tyCon) =
    "Duplicate constructor name: " <> prettyPrint tyCon
  prettyPrint (CannotFindValues names) =
    "Cannot find values: " <> prettyPrint names
  prettyPrint (CannotFindTypes names) =
    "Cannot find types: " <> prettyPrint names
  prettyPrint (CannotFindConstructor tyCon) =
    "Cannot find constructor: " <> prettyPrint tyCon
  prettyPrint (DefDoesNotTypeCheck _ name typeErr) =
    prettyPrint name <> " had a typechecking error: " <> prettyPrint typeErr
  prettyPrint (MissingModule mHash) =
    "Could not find module for " <> prettyPrint mHash
  prettyPrint (DefinitionConflictsWithImport name mHash) =
    "Cannot define " <> prettyPrint name <> " as it is already defined in import " <> prettyPrint mHash
  prettyPrint (TypeConflictsWithImport typeName mHash) =
    "Cannot define type " <> prettyPrint typeName <> " as it is already defined in import " <> prettyPrint mHash
  prettyPrint (MissingModuleDep name mHash) =
    "Cannot find dep " <> prettyPrint name <> " in module " <> prettyPrint mHash
  prettyPrint (MissingModuleTypeDep typeName mHash) =
    "Cannot find type " <> prettyPrint typeName <> " in module " <> prettyPrint mHash
  prettyPrint (DefMissingReturnType defName) =
    "Definition " <> prettyPrint defName <> " was expected to have a return type but it is missing"
  prettyPrint (DefMissingTypeAnnotation defName name) =
    "Argument " <> prettyPrint name <> " in " <> prettyPrint defName <> " was expected to have a type annotation but it does not."
  prettyPrint (EmptyTestName expr) =
    "Test name must be non-empty for expression " <> prettyPrint expr

moduleErrorDiagnostic :: ModuleError -> Diagnostic Text
moduleErrorDiagnostic (DefDoesNotTypeCheck input _ typeErr) = typeErrorDiagnostic input typeErr
moduleErrorDiagnostic other =
  let report =
        err
          Nothing
          (prettyPrint other)
          []
          []
   in addReport def report
