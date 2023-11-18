{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Smol.Modules.Dependencies
  ( getDependencies,
    filterExprs,
    filterDataTypes,
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
import Smol.Core
import Smol.Modules.Types.DefIdentifier
import Smol.Modules.Types.DepType
import qualified Smol.Modules.Types.Entity as E
import Smol.Modules.Types.Module
import Smol.Modules.Types.ModuleError
import Smol.Modules.Types.Test
import Smol.Modules.Types.TopLevelExpression
import Smol.Modules.Uses

filterExprs :: Map k (DepType dep ann) -> Map k (TopLevelExpression dep ann)
filterExprs =
  M.mapMaybe
    ( \case
        (DTExpr tle) -> Just tle
        _ -> Nothing
    )

filterDataTypes :: Map k (DepType dep ann) -> Map k (DataType dep ann)
filterDataTypes =
  M.mapMaybe
    ( \case
        (DTData dt) -> Just dt
        _ -> Nothing
    )

filterDefs :: Set E.Entity -> Set Identifier
filterDefs =
  S.fromList
    . mapMaybe
      ( \case
          E.EVar name -> Just name
          _ -> Nothing
      )
    . S.toList

filterConstructors :: Set E.Entity -> Set Constructor
filterConstructors =
  S.fromList
    . mapMaybe
      ( \case
          E.EConstructor tyCon -> Just tyCon
          _ -> Nothing
      )
    . S.toList

filterTypes :: Set E.Entity -> Set TypeName
filterTypes =
  S.fromList
    . mapMaybe
      ( \case
          E.EType typeName -> Just typeName
          _ -> Nothing
      )
    . S.toList

-- get the vars used by each def
-- explode if there's not available
getDependencies ::
  (MonadError ResolveDepsError m, Monoid ann) =>
  (Expr ParseDep ann -> Set E.Entity) ->
  (Type ParseDep ann -> Set E.Entity) ->
  Module ParseDep ann ->
  m
    ( Map
        (DefIdentifier ParseDep)
        ( DepType ParseDep ann,
          Set (DefIdentifier ParseDep),
          Set E.Entity
        )
    )
getDependencies getUses getTypeUses' mod' = do
  exprDeps <-
    M.mapKeys DIName
      <$> traverse
        (getTopLevelExprDependencies getUses mod')
        (moExpressions mod')
  typeDeps <-
    M.mapKeys DIType
      <$> traverse
        (getTypeDependencies mod')
        (moDataTypes mod')
  instanceDeps <-
    M.mapKeys DIInstance
      <$> traverse
        (getInstanceDependencies getUses getTypeUses' mod')
        (addKeysToMap $ moInstances mod')
  testDeps <-
    M.mapKeys DITest
      <$> traverse
        (getTestDependencies getUses mod')
        (M.fromList ((\(UnitTest testName expr) -> (testName, expr)) <$> moTests mod'))
  pure (exprDeps <> typeDeps <> instanceDeps <> testDeps)

addKeysToMap :: (Ord k) => M.Map k a -> M.Map k (k, a)
addKeysToMap = M.fromList . fmap (\(k, a) -> (k, (k, a))) . M.toList

-- get all dependencies of a type definition
getTypeDependencies ::
  (MonadError ResolveDepsError m) =>
  Module ParseDep ann ->
  DataType ParseDep ann ->
  m (DepType ParseDep ann, Set (DefIdentifier ParseDep), Set E.Entity)
getTypeDependencies mod' dt = do
  let allUses = extractDataTypeUses dt
  typeDefIds <- getTypeUses mod' allUses
  exprDefIds <- S.map DIName <$> getExprDeps mod' allUses
  pure (DTData dt, typeDefIds <> exprDefIds, allUses)

getTypeUses ::
  ( MonadError ResolveDepsError m,
    Ord (dep Constructor),
    Ord (dep TypeName),
    Ord (dep Identifier)
  ) =>
  Module dep ann ->
  Set E.Entity ->
  m (Set (DefIdentifier dep))
getTypeUses mod' uses =
  let typeDeps = filterTypes uses
      unknownTypeDeps =
        S.filter
          ( \typeName ->
              S.notMember typeName (M.keysSet (moDataTypes mod'))
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
        else throwError (CannotFindTypes unknownTypeDeps)

findTypenameInModule ::
  Module dep ann ->
  Constructor ->
  Maybe TypeName
findTypenameInModule mod' tyCon =
  let lookupInDataType (DataType typeName _ constructors) =
        if M.member tyCon constructors
          then First (Just typeName)
          else First Nothing
   in getFirst $ foldMap lookupInDataType (M.elems (moDataTypes mod'))

-- get typenames where we can, ignore missing ones as they're from another
-- module
-- (fingers crosseD!???!)
findTypesForConstructors ::
  Module dep ann ->
  Set Constructor ->
  Set TypeName
findTypesForConstructors mod' =
  S.fromList . mapMaybe (findTypenameInModule mod') . S.toList

getConstructorUses ::
  ( MonadError ResolveDepsError m,
    Ord (dep Constructor),
    Ord (dep Identifier),
    Ord (dep TypeName)
  ) =>
  Module dep ann ->
  Set E.Entity ->
  m (Set (DefIdentifier dep))
getConstructorUses mod' uses = do
  let typeDeps = findTypesForConstructors mod' (filterConstructors uses)
  let unknownTypeDeps =
        S.filter
          ( \typeName ->
              S.notMember typeName (M.keysSet (moDataTypes mod'))
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
        else throwError (CannotFindTypes unknownTypeDeps)

getTopLevelExprDependencies ::
  (MonadError ResolveDepsError m, Ord (dep Constructor), Ord (dep TypeName), Ord (dep Identifier)) =>
  (Expr dep ann -> Set E.Entity) ->
  Module dep ann ->
  TopLevelExpression dep ann ->
  m (DepType dep ann, Set (DefIdentifier dep), Set E.Entity)
getTopLevelExprDependencies getUses mod' expr = do
  (defIds, entities) <- getExprDependencies getUses mod' (tleExpr expr)
  pure (DTExpr expr, defIds, entities)

getTestDependencies ::
  (MonadError ResolveDepsError m, Ord (dep Constructor), Ord (dep TypeName), Ord (dep Identifier)) =>
  (Expr dep ann -> Set E.Entity) ->
  Module dep ann ->
  Expr dep ann ->
  m (DepType dep ann, Set (DefIdentifier dep), Set E.Entity)
getTestDependencies getUses mod' expr = do
  (defIds, entities) <- getExprDependencies getUses mod' expr
  pure (DTTest expr, defIds, entities)

getExprDependencies ::
  (MonadError ResolveDepsError m, Ord (dep Constructor), Ord (dep TypeName), Ord (dep Identifier)) =>
  (Expr dep ann -> Set E.Entity) ->
  Module dep ann ->
  Expr dep ann ->
  m (Set (DefIdentifier dep), Set E.Entity)
getExprDependencies getUses mod' expr = do
  let allUses = getUses expr
  exprDefIds <- S.map DIName <$> getExprDeps mod' allUses
  consDefIds <- getConstructorUses mod' allUses
  typeDefIds <- getTypeUses mod' allUses
  pure (exprDefIds <> typeDefIds <> consDefIds, allUses)

getInstanceDependencies ::
  ( MonadError ResolveDepsError m,
    Monoid ann,
    Ord (dep Constructor),
    Ord (dep TypeName),
    Ord (dep Identifier)
  ) =>
  (Expr dep ann -> Set E.Entity) ->
  (Type dep ann -> Set E.Entity) ->
  Module dep ann ->
  (Constraint dep (), Instance dep ann) ->
  m (DepType dep ann, Set (DefIdentifier dep), Set E.Entity)
getInstanceDependencies getUses getTypeUses' mod' (constraint, inst) = do
  -- get everything mentioned in instance expression
  let allUses = getUses (inExpr inst)
  exprDefIds <- S.map DIName <$> getExprDeps mod' allUses
  consDefIds <- getConstructorUses mod' allUses
  typeDefIds <- getTypeUses mod' allUses
  -- get types mentioned in constraint
  let typeUses = foldMap getTypeUses' ((fmap . fmap . fmap) (const mempty) conType constraint)
  constraintDefIds <- getTypeUses mod' typeUses
  pure (DTInstance inst, exprDefIds <> typeDefIds <> consDefIds <> constraintDefIds, allUses)

getExprDeps ::
  (Monad m) =>
  Module dep ann ->
  Set E.Entity ->
  m (Set Identifier)
getExprDeps mod' uses =
  pure $
    S.filter
      ( `S.member`
          M.keysSet (moExpressions mod')
      )
      (filterDefs uses)
