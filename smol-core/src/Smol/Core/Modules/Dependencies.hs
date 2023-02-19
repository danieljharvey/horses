{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Modules.Dependencies
  ( getDependencies,
    getModuleDeps,
    filterExprs,
    filterDataTypes,
    DepType (..),
  )
where

-- work out the dependencies between definitions inside a module

import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid (First (..))
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Core
import Language.Mimsa.Modules.HashModule
import Language.Mimsa.Modules.Monad
import Language.Mimsa.Modules.Uses
import Language.Mimsa.Types.Error

data DepType ann
  = DTExpr (Expr Name ann)
  | DTData DataType
  deriving stock (Eq, Ord, Show)

instance (Printer ann) => Printer (DepType ann) where
  prettyPrint (DTExpr expr) = prettyPrint expr
  prettyPrint (DTData dt) = prettyPrint dt

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

filterConstructors :: Set Entity -> Set TyCon
filterConstructors =
  S.fromList
    . mapMaybe
      ( \case
          EConstructor tyCon -> Just tyCon
          _ -> Nothing
      )
    . S.toList

filterTypes :: Set Entity -> Set TypeName
filterTypes =
  S.fromList
    . mapMaybe
      ( \case
          EType typeName -> Just typeName
          _ -> Nothing
      )
    . S.toList

-- get the vars used by each def
-- explode if there's not available
getDependencies ::
  (MonadError (Error Annotation) m) =>
  (Expr Name ann -> Set Entity) ->
  Module ann ->
  m
    ( Map
        DefIdentifier
        ( DepType ann,
          Set DefIdentifier,
          Set Entity
        )
    )
getDependencies getUses mod' = do
  exprDeps <-
    traverse
      (getExprDependencies getUses mod')
      (moExpressions mod')
  typeDeps <-
    mapKeys DIType
      <$> traverse
        (getTypeDependencies mod')
        (moDataTypes mod')
  pure (exprDeps <> typeDeps)

-- get all dependencies of a type definition
getTypeDependencies ::
  (MonadError (Error Annotation) m) =>
  Module ann ->
  DataType ->
  m (DepType ann, Set DefIdentifier, Set Entity)
getTypeDependencies mod' dt = do
  let allUses = extractDataTypeUses dt
  typeDefIds <- getTypeUses mod' allUses
  exprDefIds <- getExprDeps mod' allUses
  pure (DTData dt, typeDefIds <> exprDefIds, allUses)

getTypeUses ::
  (MonadError (Error Annotation) m) =>
  Module ann ->
  Set Entity ->
  m (Set DefIdentifier)
getTypeUses mod' uses =
  let typeDeps = filterTypes uses
      unknownTypeDeps =
        S.filter
          ( \typeName ->
              S.notMember typeName (M.keysSet (moDataTypes mod'))
                && S.notMember typeName (M.keysSet (moDataTypeImports mod'))
          )
          typeDeps
   in if S.null unknownTypeDeps
        then
          let localTypeDeps =
                S.filter
                  ( \typeName ->
                      typeName `S.member` M.keysSet (moDataTypes mod')
                  )
                  typeDeps
           in pure (S.map DIType localTypeDeps)
        else throwError (ModuleErr (CannotFindTypes unknownTypeDeps))

findTypenameInModule ::
  Module ann ->
  TyCon ->
  Maybe TypeName
findTypenameInModule mod' tyCon =
  let lookupInDataType (DataType typeName _ constructors) =
        if M.member tyCon constructors then First (Just typeName) else First Nothing
   in getFirst $ foldMap lookupInDataType (M.elems (moDataTypes mod'))

-- get typenames where we can, ignore missing ones as they're from another
-- module
-- (fingers crosseD!???!)
findTypesForConstructors :: Module ann -> Set TyCon -> Set TypeName
findTypesForConstructors mod' =
  S.fromList . mapMaybe (findTypenameInModule mod') . S.toList

getConstructorUses ::
  (MonadError (Error Annotation) m) =>
  Module ann ->
  Set Entity ->
  m (Set DefIdentifier)
getConstructorUses mod' uses = do
  let typeDeps = findTypesForConstructors mod' (filterConstructors uses)
  let unknownTypeDeps =
        S.filter
          ( \typeName ->
              S.notMember typeName (M.keysSet (moDataTypes mod'))
                && S.notMember typeName (M.keysSet (moDataTypeImports mod'))
          )
          typeDeps
   in if S.null unknownTypeDeps
        then
          let localTypeDeps =
                S.filter
                  ( \typeName ->
                      typeName `S.member` M.keysSet (moDataTypes mod')
                  )
                  typeDeps
           in pure (S.map DIType localTypeDeps)
        else throwError (ModuleErr (CannotFindTypes unknownTypeDeps))

getExprDependencies ::
  (MonadError (Error Annotation) m) =>
  (Expr Name ann -> Set Entity) ->
  Module ann ->
  Expr Name ann ->
  m (DepType ann, Set DefIdentifier, Set Entity)
getExprDependencies getUses mod' expr = do
  let allUses = getUses expr
  exprDefIds <- getExprDeps mod' allUses
  consDefIds <- getConstructorUses mod' allUses
  typeDefIds <- getTypeUses mod' allUses
  pure (DTExpr expr, exprDefIds <> typeDefIds <> consDefIds, allUses)

getExprDeps ::
  (MonadError (Error Annotation) m) =>
  Module ann ->
  Set Entity ->
  m (Set DefIdentifier)
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
              <> M.elems (moDataTypeImports inputModule)
          )
      mHash = snd $ serializeModule inputModule

  -- recursively fetch sub-deps
  depModules <- traverse (lookupModule moduleDeps) (S.toList deps)
  subDeps <- traverse (getModuleDeps moduleDeps) depModules

  pure $ M.singleton mHash (inputModule, deps) <> mconcat subDeps
