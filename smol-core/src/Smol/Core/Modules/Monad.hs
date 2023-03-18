{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Modules.Monad
  ( lookupModule,
    lookupModuleDep,
    lookupModuleType,
    errorIfExpressionAlreadyDefined,
    checkDataType,
    errorIfImportAlreadyDefined,
    errorIfTypeImportAlreadyDefined,
  )
where

import Control.Monad.Except
import Data.Coerce
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Smol.Core
import Smol.Core.Modules.ModuleError
import Smol.Core.Types.Module.DefIdentifier
import Smol.Core.Types.Module.Module
import Smol.Core.Types.Module.ModuleHash

lookupModule ::
  (MonadError ModuleError m) =>
  Map ModuleHash (Module ann) ->
  ModuleHash ->
  m (Module ann)
lookupModule mods modHash = do
  case M.lookup modHash mods of
    Just foundModule -> pure foundModule
    _ -> throwError (MissingModule modHash)

lookupModuleDep ::
  (MonadError ModuleError m) =>
  Map ModuleHash (Module (Type ParseDep Annotation)) ->
  DefIdentifier ->
  ModuleHash ->
  m (Expr ParseDep (Type ParseDep Annotation))
lookupModuleDep typecheckedModules def modHash = do
  case M.lookup modHash typecheckedModules of
    Just mod' ->
      case M.lookup def (moExpressions mod') of
        Just expr -> pure expr
        _ -> throwError (MissingModuleDep def modHash)
    _ -> throwError (MissingModule modHash)

lookupModuleType ::
  (MonadError ModuleError m) =>
  Map ModuleHash (Module (Type ParseDep Annotation)) ->
  TypeName ->
  ModuleHash ->
  m (DataType ParseDep Annotation)
lookupModuleType typecheckedModules typeName modHash = do
  case M.lookup modHash typecheckedModules of
    Just mod' ->
      case M.lookup typeName (moDataTypes mod') of
        Just _dt -> pure undefined -- dt
        _ -> throwError (MissingModuleTypeDep typeName modHash)
    _ -> throwError (MissingModule modHash)

errorIfExpressionAlreadyDefined ::
  (MonadError ModuleError m) =>
  Module ann ->
  DefIdentifier ->
  m ()
errorIfExpressionAlreadyDefined mod' def =
  when
    ( M.member def (moExpressions mod')
        || M.member def (moExpressionImports mod')
    )
    (throwError (DuplicateDefinition def))

checkDataType ::
  (MonadError ModuleError m) =>
  Module ann ->
  DataType ParseDep ann ->
  m ()
checkDataType mod' (DataType typeName _ constructors) = do
  errorIfTypeAlreadyDefined mod' (coerce typeName)
  traverse_ (errorIfConstructorAlreadyDefined mod') (M.keys constructors)

errorIfTypeAlreadyDefined ::
  (MonadError ModuleError m) =>
  Module ann ->
  TypeName ->
  m ()
errorIfTypeAlreadyDefined mod' typeName =
  when
    ( M.member typeName (moDataTypes mod')
        || M.member typeName (moDataTypeImports mod')
    )
    (throwError (DuplicateTypeName typeName))

errorIfConstructorAlreadyDefined ::
  (MonadError ModuleError m) =>
  Module ann ->
  Constructor ->
  m ()
errorIfConstructorAlreadyDefined mod' tyCon =
  let allCons = mconcat (M.keysSet . dtConstructors <$> M.elems (moDataTypes mod'))
   in when
        (S.member tyCon allCons)
        (throwError (DuplicateConstructor tyCon))

errorIfImportAlreadyDefined ::
  (MonadError ModuleError m) =>
  Module ann ->
  DefIdentifier ->
  ModuleHash ->
  m ()
errorIfImportAlreadyDefined mod' def moduleHash =
  when
    ( M.member def (moExpressions mod')
        || M.member def (moExpressionImports mod')
    )
    (throwError (DefinitionConflictsWithImport def moduleHash))

errorIfTypeImportAlreadyDefined ::
  (MonadError ModuleError m) =>
  Module ann ->
  TypeName ->
  ModuleHash ->
  m ()
errorIfTypeImportAlreadyDefined mod' typeName moduleHash =
  when
    ( M.member typeName (moDataTypes mod')
        || M.member typeName (moDataTypeImports mod')
    )
    (throwError (TypeConflictsWithImport typeName moduleHash))
