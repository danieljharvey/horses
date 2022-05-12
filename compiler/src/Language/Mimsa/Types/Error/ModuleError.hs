{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.ModuleError (ModuleError (..)) where

import Data.Set (Set)
import Language.Mimsa.Printer
import Language.Mimsa.Types.Error.TypeError
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Identifiers.TypeName

data ModuleError
  = DuplicateDefinition Name
  | DuplicateTypeName TypeName
  | DuplicateConstructor TyCon
  | CannotFindValues (Set Name)
  | DefDoesNotTypeCheck Name TypeError
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
