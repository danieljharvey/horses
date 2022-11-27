{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Modules.Monad
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
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.DefIdentifier
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Types.Typechecker

lookupModule ::
  (MonadError (Error Annotation) m) =>
  Map ModuleHash (Module ann) ->
  ModuleHash ->
  m (Module ann)
lookupModule mods modHash = do
  case M.lookup modHash mods of
    Just foundModule -> pure foundModule
    _ -> throwError (ModuleErr (MissingModule modHash))

lookupModuleDep ::
  (MonadError (Error Annotation) m) =>
  Map ModuleHash (Module (Type Annotation)) ->
  DefIdentifier ->
  ModuleHash ->
  m (Expr Name (Type Annotation))
lookupModuleDep typecheckedModules def modHash = do
  case M.lookup modHash typecheckedModules of
    Just mod' ->
      case M.lookup def (moExpressions mod') of
        Just expr -> pure expr
        _ -> throwError (ModuleErr (MissingModuleDep def modHash))
    _ -> throwError (ModuleErr (MissingModule modHash))

lookupModuleType ::
  (MonadError (Error Annotation) m) =>
  Map ModuleHash (Module (Type Annotation)) ->
  TypeName ->
  ModuleHash ->
  m DataType
lookupModuleType typecheckedModules typeName modHash = do
  case M.lookup modHash typecheckedModules of
    Just mod' ->
      case M.lookup typeName (moDataTypes mod') of
        Just dt -> pure dt
        _ -> throwError (ModuleErr (MissingModuleTypeDep typeName modHash))
    _ -> throwError (ModuleErr (MissingModule modHash))

errorIfExpressionAlreadyDefined ::
  (MonadError (Error Annotation) m) =>
  Module ann ->
  DefIdentifier ->
  m ()
errorIfExpressionAlreadyDefined mod' def =
  when
    ( M.member def (moExpressions mod')
        || M.member def (moExpressionImports mod')
    )
    (throwError (ModuleErr $ DuplicateDefinition def))

checkDataType ::
  (MonadError (Error Annotation) m) =>
  Module ann ->
  DataType ->
  m ()
checkDataType mod' (DataType typeName _ constructors) = do
  errorIfTypeAlreadyDefined mod' (coerce typeName)
  traverse_ (errorIfConstructorAlreadyDefined mod') (M.keys constructors)

errorIfTypeAlreadyDefined ::
  (MonadError (Error Annotation) m) =>
  Module ann ->
  TypeName ->
  m ()
errorIfTypeAlreadyDefined mod' typeName =
  when
    ( M.member typeName (moDataTypes mod')
        || M.member typeName (moDataTypeImports mod')
    )
    (throwError (ModuleErr $ DuplicateTypeName typeName))

errorIfConstructorAlreadyDefined ::
  (MonadError (Error Annotation) m) =>
  Module ann ->
  TyCon ->
  m ()
errorIfConstructorAlreadyDefined mod' tyCon =
  let allCons = mconcat (M.keysSet . dtConstructors <$> M.elems (moDataTypes mod'))
   in when
        (S.member tyCon allCons)
        (throwError (ModuleErr $ DuplicateConstructor tyCon))

errorIfImportAlreadyDefined ::
  (MonadError (Error Annotation) m) =>
  Module ann ->
  DefIdentifier ->
  ModuleHash ->
  m ()
errorIfImportAlreadyDefined mod' def moduleHash =
  when
    ( M.member def (moExpressions mod')
        || M.member def (moExpressionImports mod')
    )
    (throwError (ModuleErr $ DefinitionConflictsWithImport def moduleHash))

errorIfTypeImportAlreadyDefined ::
  (MonadError (Error Annotation) m) =>
  Module ann ->
  TypeName ->
  ModuleHash ->
  m ()
errorIfTypeImportAlreadyDefined mod' typeName moduleHash =
  when
    ( M.member typeName (moDataTypes mod')
        || M.member typeName (moDataTypeImports mod')
    )
    (throwError (ModuleErr $ TypeConflictsWithImport typeName moduleHash))
