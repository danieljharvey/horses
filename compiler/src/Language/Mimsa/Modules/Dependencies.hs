{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Modules.Dependencies (getValueDependencies, getModuleDeps, filterExprs, filterDataTypes, DepType (..)) where

-- work out the dependencies between definitions inside a module

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Modules.Uses
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Modules.Entity
import Language.Mimsa.Utils

data DepType ann
  = DTExpr (Expr Name ann)
  | DTData DataType
  deriving stock (Eq, Ord, Show)

filterExprs :: Map k (DepType ann) -> Map k (Expr Name ann)
filterExprs =
  M.mapMaybe
    ( \case
        (DTExpr expr) -> Just expr
        _ -> Nothing
    )

filterDataTypes :: Map k (DepType ann) -> Map k DataType
filterDataTypes =
  M.mapMaybe
    ( \case
        (DTData dt) -> Just dt
        _ -> Nothing
    )

filterDefs :: Set Entity -> Set DefIdentifier
filterDefs =
  S.fromList
    . mapMaybe
      ( \case
          EName name -> Just (DIName name)
          EInfix infixOp -> Just (DIInfix infixOp)
          _ -> Nothing
      )
    . S.toList

filterTypes :: Set Entity -> Set (Maybe ModuleName, TypeName)
filterTypes =
  S.fromList
    . mapMaybe
      ( \case
          EType typeName -> Just (Nothing, typeName)
          ENamespacedType modName typeName -> Just (Just modName, typeName)
          _ -> Nothing
      )
    . S.toList

-- get the vars used by each def
-- explode if there's not available
getValueDependencies ::
  (Eq ann, Monoid ann) =>
  Module ann ->
  CheckM
    ( Map
        DefIdentifier
        ( DepType ann,
          Set DefIdentifier,
          Set Entity
        )
    )
getValueDependencies mod' = do
  exprDeps <-
    traverse
      (getExprDependencies mod')
      (moExpressions mod')
  typeDeps <-
    mapKeys DIType
      <$> traverse
        (getTypeDependencies mod')
        (moDataTypes mod')
  pure (exprDeps <> typeDeps)

-- get all dependencies of a type
getTypeDependencies ::
  Module ann ->
  DataType ->
  CheckM (DepType ann, Set DefIdentifier, Set Entity)
getTypeDependencies mod' dt = do
  let allUses = extractDataTypeUses dt
  typeDefIds <- getTypeUses mod' allUses
  exprDefIds <- getExprDeps mod' allUses
  pure (DTData dt, typeDefIds <> exprDefIds, allUses)

getTypeUses :: Module ann -> Set Entity -> CheckM (Set DefIdentifier)
getTypeUses mod' uses =
  let typeDeps = filterTypes uses
      unknownTypeDeps =
        S.filter
          ( \(modName, typeName) ->
              case modName of
                Just _externalMod -> False
                Nothing ->
                  S.notMember typeName (M.keysSet (moDataTypes mod'))
                    && S.notMember typeName (M.keysSet (moDataTypeImports mod'))
          )
          typeDeps
   in if S.null unknownTypeDeps
        then
          let localTypeDeps =
                S.filter
                  ( \(modName, typeName) -> case modName of
                      Just _externalMod -> False
                      Nothing -> typeName `S.member` M.keysSet (moDataTypes mod')
                  )
                  typeDeps
              withoutExternal = localsOnly localTypeDeps
           in pure (S.map DIType withoutExternal)
        else throwError (ModuleErr (CannotFindTypes unknownTypeDeps))

localsOnly :: (Ord b) => Set (Maybe a, b) -> Set b
localsOnly =
  setMapMaybe
    ( \case
        (Just _, _) -> Nothing
        (Nothing, b) -> Just b
    )

getExprDependencies ::
  (Eq ann, Monoid ann) =>
  Module ann ->
  Expr Name ann ->
  CheckM (DepType ann, Set DefIdentifier, Set Entity)
getExprDependencies mod' expr = do
  let allUses = extractUses expr
  exprDefIds <- getExprDeps mod' allUses
  typeDefIds <- getTypeUses mod' allUses
  pure (DTExpr expr, exprDefIds <> typeDefIds, allUses)

getExprDeps ::
  Module ann ->
  Set Entity ->
  CheckM (Set DefIdentifier)
getExprDeps mod' uses =
  let nameDeps = filterDefs uses
      unknownNameDeps =
        S.filter
          ( \dep ->
              S.notMember dep (M.keysSet (moExpressions mod'))
                && S.notMember dep (M.keysSet (moExpressionImports mod'))
          )
          nameDeps
   in if S.null unknownNameDeps
        then
          let localNameDeps =
                S.filter
                  ( `S.member`
                      M.keysSet (moExpressions mod')
                  )
                  nameDeps
           in pure localNameDeps
        else throwError (ModuleErr (CannotFindValues unknownNameDeps))

-- starting at a root module,
-- create a map of each expr hash along with the modules it needs
-- so that we can typecheck them all
getModuleDeps ::
  (MonadError (Error Annotation) m, Show ann) =>
  Map ModuleHash (Module ann) ->
  Module ann ->
  m
    ( Map
        ModuleHash
        ( Module ann,
          Set ModuleHash
        )
    )
getModuleDeps moduleDeps inputModule = do
  -- get this module's deps
  let deps =
        S.fromList
          ( M.elems (moExpressionImports inputModule)
              <> M.elems (moNamedImports inputModule)
          )
      mHash = snd $ serializeModule inputModule
  -- recursively fetch sub-deps
  depModules <- traverse (lookupModule moduleDeps) (S.toList deps)
  subDeps <- traverse (getModuleDeps moduleDeps) depModules

  pure $ M.singleton mHash (inputModule, deps) <> mconcat subDeps
