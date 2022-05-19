{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.ModuleError (ModuleError (..)) where

import Data.Set (Set)
import Language.Mimsa.Printer
import Language.Mimsa.Types.Error.TypeError
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Identifiers.TypeName
import Language.Mimsa.Types.Modules.ModuleHash

data ModuleError
  = DuplicateDefinition Name
  | DuplicateTypeName TypeName
  | DuplicateConstructor TyCon
  | DefinitionConflictsWithImport Name ModuleHash
  | TypeConflictsWithImport TypeName ModuleHash
  | CannotFindValues (Set Name)
  | DefDoesNotTypeCheck Name TypeError
  | MissingModule ModuleHash
  | MissingModuleDep Name ModuleHash
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
  prettyPrint (DefDoesNotTypeCheck name typeErr) =
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
