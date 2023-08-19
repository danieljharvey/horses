{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smol.Core.Modules.FromParts (addModulePart, moduleFromModuleParts, exprAndTypeFromParts) where

import Control.Monad.Except
import Data.Coerce
import Data.Functor (void)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, mapMaybe)
import Smol.Core
import Smol.Core.Modules.Monad
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
    ModuleExpression name bits expr -> do
      errorIfExpressionAlreadyDefined mod' name
      let exp' = exprAndTypeFromParts allParts name bits expr
      pure $
        mod'
          { moExpressions =
              M.singleton name exp' <> moExpressions mod'
          }
    ModuleExpressionType _name _ _ty -> do
      pure mod' -- we sort these elsewhere
    ModuleTest testName ident
      | "" == testName ->
          throwError (EmptyTestName ident)
    ModuleTest testName ident ->
      if expressionExists ident allParts
        then
          pure $
            mod'
              { moTests = UnitTest testName ident : moTests mod'
              }
        else throwError (ErrorInResolveDeps $ VarNotFound ident)
    ModuleClass tc ->
      -- TODO: check duplicates and explode
      pure $ mod' {moClasses = M.singleton (tcName tc) tc <> moClasses mod'}
    ModuleInstance constraint expr ->
      pure $
        mod'
          { moInstances =
              M.singleton
                (void constraint)
                ( Instance
                    { inConstraints = mempty,
                      inExpr = expr
                    }
                )
                <> moInstances mod'
          }
    ModuleDataType dt@(DataType tyCon _ _) -> do
      let typeName = coerce tyCon
      checkDataType mod' dt
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
  (Monoid ann) =>
  [ModuleItem ann] ->
  Identifier ->
  [Identifier] ->
  Expr ParseDep ann ->
  TopLevelExpression ParseDep ann
exprAndTypeFromParts moduleItems ident idents expr =
  let tleExpr =
        foldr
          (ELambda mempty . emptyParseDep)
          expr
          idents
      (tleConstraints, tleType) =
        case findTypeExpression ident moduleItems of
          Just (constraints, ty) ->
            (constraints, Just ty)
          Nothing ->
            (mempty, Nothing)
   in TopLevelExpression {..}

findExpression ::
  (Monoid ann) =>
  Identifier ->
  [ModuleItem ann] ->
  Maybe (TopLevelExpression ParseDep ann)
findExpression ident moduleItems =
  case mapMaybe
    ( \case
        ModuleExpression name bits expr
          | name == ident ->
              Just (exprAndTypeFromParts moduleItems name bits expr)
        _ -> Nothing
    )
    moduleItems of
    [a] -> Just a
    _ -> Nothing -- we should have better errors for multiple declarations, but for now, chill out friend

expressionExists :: (Monoid ann) => Identifier -> [ModuleItem ann] -> Bool
expressionExists ident moduleItems = isJust (findExpression ident moduleItems)

findTypeExpression :: Identifier -> [ModuleItem ann] -> Maybe ([Constraint ParseDep ann], Type ParseDep ann)
findTypeExpression ident moduleItems =
  case mapMaybe
    ( \case
        ModuleExpressionType name constraints ty | name == ident -> Just (constraints, ty)
        _ -> Nothing
    )
    moduleItems of
    [a] -> Just a
    _ -> Nothing -- we should have better errors for multiple type declarations, but for now, chill out friend
