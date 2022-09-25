{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Modules.Evaluate
  ( evaluateModule,
  )
where

import Control.Monad.Except
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Interpret as Actions
import qualified Language.Mimsa.Actions.Modules.ToStoreExpressions as Actions
import qualified Language.Mimsa.Actions.Modules.Typecheck as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Modules.Check
import Language.Mimsa.Modules.ToStoreExprs
import Language.Mimsa.Modules.Uses
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Modules.Entity
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

-- we need to bind our new expression to _something_
-- so we make a `Name` which is strictly broken, but it means
-- we won't have name collisions with a real expression
evalId :: DefIdentifier
evalId = DIName (Name "_repl")

-- given deps that this expression requires, attempt to resolve these into
-- imports we'll need from the Project environment
importsFromEntities :: Set Entity -> Actions.ActionM (Module Annotation)
importsFromEntities uses = do
  prj <- Actions.getProject
  let fromEntity = \case
        ENamespacedName modName _ ->
          case lookupModuleName prj modName of
            Right modHash -> pure $ mempty {moNamedImports = M.singleton modName modHash}
            Left found -> throwError (ProjectErr (CannotFindModuleByName modName found))
        ENamespacedType modName _ ->
          case lookupModuleName prj modName of
            Right modHash -> pure $ mempty {moNamedImports = M.singleton modName modHash}
            Left found -> throwError (ProjectErr (CannotFindModuleByName modName found))
        ENamespacedConstructor modName _ ->
          case lookupModuleName prj modName of
            Right modHash -> pure $ mempty {moNamedImports = M.singleton modName modHash}
            Left found -> throwError (ProjectErr (CannotFindModuleByName modName found))
        _ -> pure mempty
  -- check them all, combine them
  mconcat <$> traverse fromEntity (S.toList uses)

entitiesFromModule :: (Eq ann) => Module ann -> Set Entity
entitiesFromModule localModule =
  foldMap extractUses (M.elems (moExpressions localModule))

-- when we evaluate an expression, really we are adding it to an open module
-- then evaluating the expression in the context of that module
-- this means we can bind successive values
-- we should probably stop users adding the same type/value twice so we don't
-- have to deal with all the confusion
--
-- 1. work out what stuff the expression uses
-- 2. turns those into a module (ie, implied imports, new defs etc)
-- 3. combine that with the local module
-- 4. typecheck it
-- 5. get the expression type from the module type
-- 6. compile into store expressions
-- 6. interpret store expressions as normal
evaluateModule ::
  Expr Name Annotation ->
  Module Annotation ->
  Actions.ActionM (MonoType, Expr Name Annotation, Module Annotation)
evaluateModule expr localModule = do
  -- work out implied imports
  moduleImports <-
    importsFromEntities
      ( extractUses expr
          <> entitiesFromModule localModule
      )

  -- make a module for it, adding our expression as _repl
  let newModule =
        localModule
          <> mempty
            { moExpressions =
                M.singleton evalId expr,
              moExpressionExports = S.singleton evalId
            }
          <> moduleImports

  -- typecheck it
  typecheckedModule <- Actions.typecheckModule (prettyPrint newModule) newModule

  -- compile to store expressions
  compiled <- Actions.toStoreExpressions typecheckedModule

  -- find the root StoreExpression by name
  rootStoreExpr <- Actions.lookupByName compiled evalId

  -- unsafe, yolo
  let exprType = fromJust (lookupModuleDefType typecheckedModule evalId)

  -- need to get our new store items into the project so this works I reckon
  traverse_
    (Actions.appendStoreExpression . fmap getAnnotationForType)
    (getStore $ cmStore compiled)

  -- interpret
  evaluatedExpression <-
    Actions.interpreter (getAnnotationForType <$> rootStoreExpr)

  pure (exprType, evaluatedExpression, newModule)
