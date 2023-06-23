{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Smol.Core.Modules.FromParts (addModulePart, moduleFromModuleParts, exprAndTypeFromParts) where

import Control.Monad.Except
import Data.Coerce
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Smol.Core
import Smol.Core.Modules.ModuleError
import Smol.Core.Modules.Monad
import Smol.Core.Types.Module.DefIdentifier
import Smol.Core.Types.Module.Module
import Smol.Core.Types.Module.ModuleHash

moduleFromModuleParts ::
  ( MonadError ModuleError m,
    Monoid ann
  ) =>
  Map ModuleHash (Module ParseDep Annotation) ->
  [ModuleItem ann] ->
  m (Module ParseDep ann)
moduleFromModuleParts modules parts =
  let addPart part output = do
        mod' <- output
        addModulePart modules part mod'
   in foldr addPart (pure mempty) parts

addModulePart ::
  (MonadError ModuleError m, Monoid ann) =>
  Map ModuleHash (Module ParseDep Annotation) ->
  ModuleItem ann ->
  Module ParseDep ann ->
  m (Module ParseDep ann)
addModulePart modules part mod' =
  case part of
    ModuleExpression name bits expr -> do
      errorIfExpressionAlreadyDefined mod' (DIName name)
      let exp' = exprAndTypeFromParts bits expr
      pure $
        mod'
          { moExpressions =
              M.singleton (DIName name) exp' <> moExpressions mod'
          }
    ModuleExpressionType _name _ty -> do
      error "addModulePart ModuleExpressionType"
    ModuleDataType dt@(DataType tyCon _ _) -> do
      let typeName = coerce tyCon
      checkDataType mod' dt
      pure $
        mod'
          { moDataTypes =
              M.singleton typeName dt
                <> moDataTypes mod'
          }
    ModuleExport modItem -> do
      -- get whatever is inside
      innerModule <- addModulePart modules modItem mod'
      -- get the keys, add them to exports
      let defExports = case modItem of
            ModuleExpression name _ _ -> S.singleton (DIName name)
            _ -> mempty
      let typeExports = case modItem of
            ModuleDataType (DataType tn _ _) -> S.singleton (coerce tn)
            _ -> mempty
      pure $
        innerModule
          { moExpressionExports =
              defExports <> moExpressionExports innerModule,
            moDataTypeExports =
              typeExports <> moDataTypeExports innerModule
          }
    ModuleImport (ImportNamedFromHash mHash mName) ->
      pure $ mod' {moNamedImports = M.singleton mName mHash <> moNamedImports mod'}
    ModuleImport (ImportAllFromHash mHash) -> do
      importMod <- lookupModule modules mHash
      let defImports =
            M.fromList
              . fmap (,mHash)
              . S.toList
              . moExpressionExports
              $ importMod

      -- explode if these are defined already
      _ <-
        M.traverseWithKey
          (errorIfImportAlreadyDefined mod')
          defImports

      let typeImports =
            M.fromList
              . fmap (,mHash)
              . S.toList
              . moDataTypeExports
              $ importMod

      -- explode if these types are defined already
      _ <-
        M.traverseWithKey
          (errorIfTypeImportAlreadyDefined mod')
          typeImports

      pure $
        mod'
          { moExpressionImports =
              defImports
                <> moExpressionImports mod',
            moDataTypeImports =
              typeImports
                <> moDataTypeImports mod'
          }

-- given the bits of things, make a coherent type and expression
-- 1) check we have any type annotations
-- 2) if so - ensure we have a full set (error if not) and create annotation
-- 3) if not, just return expr
exprAndTypeFromParts ::
  (Monoid ann) =>
  [Identifier] ->
  Expr ParseDep ann ->
  Expr ParseDep ann
exprAndTypeFromParts parts expr = do
  foldr
    ( ELambda mempty . emptyParseDep
    )
    expr
    parts
