{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Smol.Core.Modules.Check
  ( checkModule,
  )
where

import Control.Monad (void)
import Control.Monad.Except
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Smol.Core
import Smol.Core.Helpers
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
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

  passModuleDictionaries input typedModule

passModuleDictionaries ::
  (MonadError (ModuleError Annotation) m) =>
  T.Text ->
  Module ResolvedDep (Type ResolvedDep Annotation) ->
  m (Module ResolvedDep (Type ResolvedDep Annotation))
passModuleDictionaries input inputModule = do
  let env = envFromTypecheckedModule inputModule

  let passDictToTopLevelExpression (ident, tle) = do
        let constraints = constraintsFromTLE tle
            expr = tleExpr tle

        tracePrettyM "module passModuleDictionaries to" ident

        let thisEnv =
              env
                { tceConstraints = constraints
                }

        let typedConstraints = addTypesToConstraint <$> constraints

        let lookupInstance =
              lookupTypecheckedTypeclassInstance thisEnv (moInstances inputModule)
                . removeTypesFromConstraint

        newExpr <-
          modifyError
            (DictionaryPassingError input)
            (toDictionaryPassing lookupInstance thisEnv typedConstraints expr)

        pure (ident, tle {tleExpr = newExpr})

  newExpressions <- M.fromList <$> traverse passDictToTopLevelExpression (M.toList $ moExpressions inputModule)
  pure $ inputModule {moExpressions = newExpressions}

-- create an instance using the already typechecked instances we already have
lookupTypecheckedTypeclassInstance ::
  forall m ann.
  (MonadError (TCError ann) m, Monoid ann, Ord ann, Show ann) =>
  TCEnv ann ->
  M.Map (Constraint ResolvedDep ()) (Instance ResolvedDep (Type ResolvedDep ann)) ->
  Constraint ResolvedDep ann ->
  m (Instance ResolvedDep (Type ResolvedDep ann))
lookupTypecheckedTypeclassInstance env instances constraint = do
  tracePrettyM "lookupTypecheckedTypeclassInstance" constraint
  case M.lookup (void constraint) instances of
    Just tcInstance -> pure tcInstance
    Nothing -> do
      (foundConstraint, subs) <- findMatchingConstraint (M.keys instances) constraint
      tracePrettyM "foundConstraint" foundConstraint
      tracePrettyM "subs" subs
      -- a) look up main instance

      case M.lookup (void foundConstraint) instances of
        Just (Instance {inConstraints, inExpr}) -> do
          tracePrettyM "found concrete generalised instance" (inConstraints, inExpr)
          -- specialise contraints to found types
          let subbedConstraints = substituteConstraint subs <$> (removeTypesFromConstraint <$> inConstraints)

          tracePrettyM "subbed constraints" subbedConstraints

          -- see if found types exist
          traverse_ (lookupTypecheckedTypeclassInstance env instances) subbedConstraints

          -- return new instance
          pure
            ( Instance
                { inConstraints, -- = addTypesToConstraint <$> subbedConstraints,
                  inExpr
                }
            )
        Nothing ->
          -- let constraintsWithAnn = (fmap . fmap) (const mempty) (M.keys instances)
          error "dict passing cant find instance" -- throwError (TCTypeclassInstanceNotFound name tys constraintsWithAnn)

-- | given a pile of constraints, find the matching one and return
-- substitutions required to make it match
-- TODO: this needs to accept TCEnv and lookup constraints in there too thx
-- TODO: make our own error types for this crap so it's less confusing what is
-- a type error or not
findMatchingConstraint ::
  forall m ann.
  (MonadError (TCError ann) m, Monoid ann) =>
  [Constraint ResolvedDep ()] ->
  Constraint ResolvedDep ann ->
  m (Constraint ResolvedDep ann, [Substitution ResolvedDep ann])
findMatchingConstraint constraints (Constraint name tys) =
  let constraintsWithAnn :: [Constraint ResolvedDep ann]
      constraintsWithAnn = (fmap . fmap) (const mempty) constraints

      lookupConstraint (Constraint innerName innerTys) =
        case (innerName == name, instanceMatchesType tys innerTys) of
          (True, Right matches) -> Just (Constraint innerName innerTys, matches)
          _ -> Nothing
   in case mapMaybe lookupConstraint constraintsWithAnn of
        -- we deliberately fail if we find more than one matching instance
        [(foundConstraint, subs)] -> pure (foundConstraint, subs)
        [] ->
          error "find match constraint problem" -- throwError (TCTypeclassInstanceNotFound name tys constraintsWithAnn)
        multiple ->
          throwError (TCConflictingTypeclassInstancesFound (fst <$> multiple))
