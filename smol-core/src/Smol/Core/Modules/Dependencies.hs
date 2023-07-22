{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Smol.Core.Modules.Dependencies
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
import Smol.Core.Modules.Types.DefIdentifier
import Smol.Core.Modules.Types.DepType
import qualified Smol.Core.Modules.Types.Entity as E
import Smol.Core.Modules.Types.Module
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Modules.Types.TopLevelExpression
import Smol.Core.Modules.Uses

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
  (MonadError (ModuleError ann) m) =>
  (Expr ParseDep ann -> Set E.Entity) ->
  Module ParseDep ann ->
  m
    ( Map
        DefIdentifier
        ( DepType ParseDep ann,
          Set DefIdentifier,
          Set E.Entity
        )
    )
getDependencies getUses mod' = do
  exprDeps <-
    M.mapKeys DIName
      <$> traverse
        (getExprDependencies getUses mod')
        (moExpressions mod')
  typeDeps <-
    M.mapKeys DIType
      <$> traverse
        (getTypeDependencies mod')
        (moDataTypes mod')
  pure (exprDeps <> typeDeps)

-- get all dependencies of a type definition
getTypeDependencies ::
  (MonadError (ModuleError ann) m) =>
  Module ParseDep ann ->
  DataType ParseDep ann ->
  m (DepType ParseDep ann, Set DefIdentifier, Set E.Entity)
getTypeDependencies mod' dt = do
  let allUses = extractDataTypeUses dt
  typeDefIds <- getTypeUses mod' allUses
  exprDefIds <- S.map DIName <$> getExprDeps mod' allUses
  pure (DTData dt, typeDefIds <> exprDefIds, allUses)

getTypeUses ::
  (MonadError (ModuleError ann) m) =>
  Module dep ann ->
  Set E.Entity ->
  m (Set DefIdentifier)
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
  (MonadError (ModuleError ann) m) =>
  Module dep ann ->
  Set E.Entity ->
  m (Set DefIdentifier)
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

getExprDependencies ::
  (MonadError (ModuleError ann) m) =>
  (Expr dep ann -> Set E.Entity) ->
  Module dep ann ->
  TopLevelExpression dep ann ->
  m (DepType dep ann, Set DefIdentifier, Set E.Entity)
getExprDependencies getUses mod' expr = do
  let allUses = getUses (tleExpr expr)
  exprDefIds <- S.map DIName <$> getExprDeps mod' allUses
  consDefIds <- getConstructorUses mod' allUses
  typeDefIds <- getTypeUses mod' allUses
  pure (DTExpr expr, exprDefIds <> typeDefIds <> consDefIds, allUses)

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
