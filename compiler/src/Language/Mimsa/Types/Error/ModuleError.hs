{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.ModuleError (ModuleError (..), moduleErrorDiagnostic) where

import Data.Set (Set)
import Data.Text (Text)
import Error.Diagnose
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.InfixOp
import Language.Mimsa.Types.Error.TypeError
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Identifiers.TypeName
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Types.Modules.Module

data ModuleError
  = DuplicateDefinition DefIdentifier
  | DuplicateTypeName TypeName
  | DuplicateConstructor TyCon
  | DefinitionConflictsWithImport DefIdentifier ModuleHash
  | TypeConflictsWithImport TypeName ModuleHash
  | CannotFindValues (Set DefIdentifier)
  | DefDoesNotTypeCheck Text Name TypeError
  | InfixDoesNotTypeCheck Text InfixOp TypeError
  | MissingModule ModuleHash
  | MissingModuleDep DefIdentifier ModuleHash
  | MissingModuleTypeDep TypeName ModuleHash
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
  prettyPrint (DefDoesNotTypeCheck _ name typeErr) =
    prettyPrint name <> " had a typechecking error: " <> prettyPrint typeErr
  prettyPrint (InfixDoesNotTypeCheck _ infixOp typeErr) =
    prettyPrint infixOp <> " had a typechecking error: " <> prettyPrint typeErr
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
