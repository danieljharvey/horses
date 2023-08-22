{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
  {-# LANGUAGE NamedFieldPuns #-}
module Smol.Core.Modules.Check
  ( checkModule,
  )
where

import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import Control.Monad (void)
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Smol.Core
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
import Smol.Core.Modules.Types.DefIdentifier
import Smol.Core.Modules.Types.Module
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Modules.Types.ModuleItem
import Smol.Core.Modules.Types.TopLevelExpression
import Smol.Core.Typecheck.Typeclass

-- this is the front door as such
checkModule ::
  (MonadError (ModuleError Annotation) m) =>
  T.Text ->
  [ModuleItem Annotation] ->
  m (Module ResolvedDep (Type ResolvedDep Annotation))
checkModule input moduleItems = do
  myModule <- moduleFromModuleParts moduleItems

  let classes = resolveTypeclass <$> moClasses myModule
      typeclassMethods = S.fromList . M.elems . fmap tcFuncName $ classes

  (resolvedModule, deps) <-
    modifyError ErrorInResolveDeps (resolveModuleDeps typeclassMethods myModule)

  typedModule <- typecheckModule input resolvedModule deps

  passModuleDictionaries typedModule

passModuleDictionaries ::
  (MonadError (ModuleError Annotation) m) =>
  Module ResolvedDep (Type ResolvedDep Annotation) ->
  m (Module ResolvedDep (Type ResolvedDep Annotation))
passModuleDictionaries inputModule = do
  let env = envFromTypecheckedModule inputModule

  let passDictToTopLevelExpression (ident, tle) = do
        let constraints = constraintsFromTLE tle
            expr = tleExpr tle

        let thisEnv =
              env
                { tceConstraints = constraints
                }

        let typedConstraints = addTypesToConstraint <$> constraints

        let lookupInstance constraint =
              case M.lookup (void constraint) (moInstances inputModule) of
                Just inst -> Right inst
                _ -> error "sdfsdf"

        newExpr <-
          modifyError
            (DefDoesNotTypeCheck mempty (DIName ident))
            (toDictionaryPassing lookupInstance thisEnv typedConstraints expr)

        pure (ident, tle {tleExpr = newExpr})

  newExpressions <- M.fromList <$> traverse passDictToTopLevelExpression (M.toList $ moExpressions inputModule)
  pure $ inputModule {moExpressions = newExpressions}


-- create an instance using the already typechecked instances we already have
lookupTypecheckedTypeclassInstance ::
  (MonadError (TCError ann) m, Monoid ann, Ord ann, Show ann) =>
  M.Map (Constraint ResolvedDep ()) (Instance ResolvedDep (Type ResolvedDep ann)) ->
  Constraint ResolvedDep ann ->
  m (Instance ResolvedDep ann)
lookupTypecheckedTypeclassInstance instances constraint@(Constraint name tys) = do
  let lookupTypecheckedConcreteInstance constraint =
        case M.lookup (void constraint) instances of
                Just inst -> Right inst
                _ -> error "instance not found"

  -- first, do we have a concrete instance?
  case lookupTypecheckedConcreteInstance constraint of
    Just tcInstance -> pure tcInstance
    Nothing -> do
      case mapMaybe
        ( \(Constraint innerName innerTys) ->
            case (innerName == name, instanceMatchesType tys innerTys) of
              (True, Right matches) -> Just (Constraint innerName innerTys, matches)
              _ -> Nothing
        )
        (M.keys instances) of
        -- we deliberately fail if we find more than one matching instance
        [(foundConstraint, subs)] -> do
          -- a) look up main instance
          case lookupTypecheckedConcreteInstance foundConstraint of
            Just (Instance {inConstraints, inExpr}) -> do
              -- specialise contraints to found types
              let subbedConstraints = substituteConstraint subs <$> inConstraints
              -- see if found types exist
              traverse_ (lookupTypecheckedTypeclassInstance instances) subbedConstraints
              -- return new instance
              pure (Instance {inConstraints = subbedConstraints, inExpr})
            Nothing ->
              throwError (TCTypeclassInstanceNotFound name tys (M.keys instances))
        [] ->
          throwError (TCTypeclassInstanceNotFound name tys (M.keys instances))
        multiple ->
          throwError (TCConflictingTypeclassInstancesFound (fst <$> multiple))


