{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smol.Modules.FromParts
  ( addModulePart,
    moduleFromModuleParts,
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
import Smol.Modules.Types.Module
import Smol.Modules.Types.ModuleError
import Smol.Modules.Types.ModuleItem
import Smol.Modules.Types.Test
import Smol.Modules.Types.TopLevelExpression

moduleFromModuleParts ::
  ( MonadError (ModuleError ann) m
  ) =>
  [ModuleItem ann] ->
  m (Module ParseDep ann)
moduleFromModuleParts parts =
  let addPart part output = do
        mod' <- output
        addModulePart parts part mod'
   in foldr addPart (pure mempty) parts

addModulePart ::
  (MonadError (ModuleError ann) m) =>
  [ModuleItem ann] ->
  ModuleItem ann ->
  Module ParseDep ann ->
  m (Module ParseDep ann)
addModulePart allParts part mod' =
  case part of
    ModuleExpression (ModuleExpressionC {meArgs, meReturnType, meExpr, meConstraints, meIdent}) -> do
      _ <- findExpression meIdent allParts 
      tle <- exprAndTypeFromParts meConstraints meArgs meReturnType meExpr
      pure $
        mod'
          { moExpressions =
              M.singleton meIdent tle <> moExpressions mod'
          }
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

findExpression ::
  (MonadError (ModuleError ann) m) =>
  Identifier ->
  [ModuleItem ann] ->
  m (Expr ParseDep ann)
findExpression ident moduleItems =
  case mapMaybe
    ( \case
        ModuleExpression moduleExpression
          | meIdent moduleExpression == ident ->
              Just moduleExpression
        _ -> Nothing
    )
    moduleItems of
    [a] -> pure (meExpr a)
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

-- given the bits of things, make a coherent type and expression
exprAndTypeFromParts ::
  (MonadError (ModuleError ann) m) =>
  [Constraint ParseDep ann] ->
  [(Identifier, Type ParseDep ann)] ->
  Type ParseDep ann ->
  Expr ParseDep ann ->
  m (TopLevelExpression ParseDep ann)
exprAndTypeFromParts constraints parts retType expr = do
  let expr' =
        foldr
          ( \(ident, ty) rest -> ELambda (getTypeAnnotation ty) (emptyParseDep ident) rest
          )
          expr
          parts
  -- if we only have un-typed args, don't bother, we only want them as
  -- placeholders
  let exprType =
        foldr
          ( \(_ident, ty) rest -> do
              TFunc (getTypeAnnotation ty) mempty ty rest
          )
          retType
          parts
  pure $
    TopLevelExpression
      { tleConstraints = constraints,
        tleExpr = expr',
        tleType = Just exprType
      }
