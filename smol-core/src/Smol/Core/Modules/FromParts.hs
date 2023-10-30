{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smol.Core.Modules.FromParts
  ( addModulePart,
    moduleFromModuleParts,
    exprAndTypeFromParts,
  )
where

import Control.Monad (unless)
import Control.Monad.Except
import Data.Coerce
import Data.Foldable (traverse_)
import Data.Functor (void)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as S
import Smol.Core
import Smol.Core.Modules.Types.Module
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Modules.Types.ModuleItem
import Smol.Core.Modules.Types.Test
import Smol.Core.Modules.Types.TopLevelExpression

moduleFromModuleParts ::
  ( MonadError (ModuleError ann) m,
    Monoid ann
  ) =>
  [ModuleItem ann] ->
  m (Module ParseDep ann)
moduleFromModuleParts parts =
  let addPart part output = do
        mod' <- output
        addModulePart parts part mod'
   in foldr addPart (pure mempty) parts

addModulePart ::
  (MonadError (ModuleError ann) m, Monoid ann) =>
  [ModuleItem ann] ->
  ModuleItem ann ->
  Module ParseDep ann ->
  m (Module ParseDep ann)
addModulePart allParts part mod' =
  case part of
    ModuleExpression (ModuleExpressionC {meIdent}) -> do
      tle <- exprAndTypeFromParts allParts meIdent
      pure $
        mod'
          { moExpressions =
              M.singleton meIdent tle <> moExpressions mod'
          }
    ModuleType (ModuleTypeC {mtIdent}) -> do
      -- check for duplicates
      _ <- findTypeExpression mtIdent allParts
      pure mod' -- we sort these elsewhere
    ModuleTest testName expr
      | "" == testName ->
          throwError (EmptyTestName expr)
    ModuleTest testName expr ->
      pure $
        mod'
          { moTests = UnitTest testName expr : moTests mod'
          }
    ModuleClass tc ->
      case M.lookup (tcName tc) (moClasses mod') of
        Just _ -> throwError (DuplicateTypeclass (tcName tc))
        Nothing ->
          pure $
            mod'
              { moClasses =
                  M.singleton (tcName tc) tc <> moClasses mod'
              }
    ModuleInstance (ModuleInstanceC {miAnn, miConstraints, miHead, miExpr}) -> do
      unless
        (isJust $ findTypeclass (conTypeclass miHead) allParts)
        (throwError $ MissingTypeclass miAnn (conTypeclass miHead))
      pure $
        mod'
          { moInstances =
              M.singleton
                (void miHead)
                ( Instance
                    { inConstraints = miConstraints,
                      inExpr = miExpr
                    }
                )
                <> moInstances mod'
          }
    ModuleDataType (ModuleDataTypeC {mdtDataType = DataType tyCon _ constructors}) -> do
      let typeName = coerce tyCon
      dt <- findDataType typeName allParts
      traverse_ (`findConstructor` allParts) (M.keys constructors)
      pure $
        mod'
          { moDataTypes =
              M.singleton typeName dt
                <> moDataTypes mod'
          }

-- given the bits of things, make a coherent type and expression
-- 1) check we have any type annotations
-- 2) if so - ensure we have a full set (error if not) and create annotation
-- 3) if not, just return expr
exprAndTypeFromParts ::
  (MonadError (ModuleError ann) m, Monoid ann) =>
  [ModuleItem ann] ->
  Identifier ->
  m (TopLevelExpression ParseDep ann)
exprAndTypeFromParts moduleItems ident = do
  (idents, expr) <- findExpression ident moduleItems
  let tleExpr =
        foldr
          (ELambda mempty . emptyParseDep)
          expr
          idents
  (tleConstraints, tleType) <- do
    foundType <- findTypeExpression ident moduleItems
    case foundType of
      Just (constraints, ty) ->
        pure (constraints, Just ty)
      Nothing ->
        pure (mempty, Nothing)
  pure $ TopLevelExpression {..}

findExpression ::
  (MonadError (ModuleError ann) m) =>
  Identifier ->
  [ModuleItem ann] ->
  m ([Identifier], Expr ParseDep ann)
findExpression ident moduleItems =
  case mapMaybe
    ( \case
        ModuleExpression moduleExpression
          | meIdent moduleExpression == ident ->
              Just moduleExpression
        _ -> Nothing
    )
    moduleItems of
    [a] -> pure (meArgs a, meExpr a)
    [] -> error "won't happen"
    (a : b : _) ->
      throwError
        ( DuplicateDefinition
            ( Duplicate
                ident
                (meAnn a)
                (meAnn b)
            )
        )

findTypeExpression ::
  (MonadError (ModuleError ann) m) =>
  Identifier ->
  [ModuleItem ann] ->
  m (Maybe ([Constraint ParseDep ann], Type ParseDep ann))
findTypeExpression ident moduleItems =
  case mapMaybe
    ( \case
        ModuleType moduleType
          | mtIdent moduleType == ident ->
              Just moduleType
        _ -> Nothing
    )
    moduleItems of
    [a] -> pure $ Just (mtConstraints a, mtType a)
    [] -> pure Nothing
    (a : b : _) ->
      throwError
        ( DuplicateTypeDefinition
            ( Duplicate
                ident
                (mtAnn a)
                (mtAnn b)
            )
        )

findDataType ::
  (MonadError (ModuleError ann) m) =>
  TypeName ->
  [ModuleItem ann] ->
  m (DataType ParseDep ann)
findDataType typeName moduleItems =
  case mapMaybe
    ( \case
        ModuleDataType mdt@ModuleDataTypeC {mdtDataType = DataType {dtName}}
          | dtName == typeName ->
              Just mdt
        _ -> Nothing
    )
    moduleItems of
    [a] -> pure (mdtDataType a)
    [] -> error "shouldn't happen"
    (a : b : _) ->
      throwError
        ( DuplicateTypeName
            ( Duplicate
                typeName
                (mdtAnn a)
                (mdtAnn b)
            )
        )

findConstructor ::
  (MonadError (ModuleError ann) m) =>
  Constructor ->
  [ModuleItem ann] ->
  m (DataType ParseDep ann)
findConstructor tyCon moduleItems =
  case mapMaybe
    ( \case
        ModuleDataType mdt@ModuleDataTypeC {mdtDataType = DataType {dtConstructors}}
          | S.member tyCon (M.keysSet dtConstructors) ->
              Just mdt
        _ -> Nothing
    )
    moduleItems of
    [a] -> pure (mdtDataType a)
    [] -> error "shouldn't happen"
    (a : b : _) ->
      throwError
        ( DuplicateConstructor
            ( Duplicate
                tyCon
                (mdtAnn a)
                (mdtAnn b)
            )
        )

findTypeclass :: TypeclassName -> [ModuleItem ann] -> Maybe (Typeclass ParseDep ann)
findTypeclass tcn moduleItems =
  case mapMaybe
    ( \case
        ModuleClass tc | tcName tc == tcn -> Just tc
        _ -> Nothing
    )
    moduleItems of
    [a] -> Just a
    _ -> Nothing -- we should have better errors for multiple type declarations, but for now, chill out friend
