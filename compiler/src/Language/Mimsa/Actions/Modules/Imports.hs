{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Modules.Imports
  ( evalId,
    importsFromEntities,
    entitiesFromModule,
    findUsesInProject,
  )
where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Modules.Uses
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Core
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Modules.Entity

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

findUsesInProject ::
  Expr Name Annotation ->
  Module Annotation ->
  Actions.ActionM (Module Annotation)
findUsesInProject expr localModule = do
  -- work out implied imports
  importsFromEntities
    ( extractUses expr
        <> entitiesFromModule localModule
    )
