{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE NamedFieldPuns #-}
-- everything used to typecheck typeclasses
module Smol.Core.Typecheck.Typeclass.Typecheck
  ( checkInstance,
    lookupInstanceAndCheck,
  )
where

import Data.Maybe
import Smol.Core.Typecheck.Typeclass.KindChecker
import Debug.Trace
import Control.Monad.Except
import qualified Data.Map.Strict as M
import Smol.Core.Typecheck.Elaborate (elaborate)
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Typeclass.Helpers
import Smol.Core.Typecheck.Types
import Smol.Core.Types

lookupInstanceAndCheck ::
  (Ord ann, Monoid ann, Show ann, MonadError (TCError ann) m) =>
  TCEnv ann ->
  Constraint ResolvedDep (Type ResolvedDep ann) ->
  m (Instance ResolvedDep (Type ResolvedDep ann))
lookupInstanceAndCheck env constraint@(Constraint typeclassName _) = do
  tcInstance <- lookupTypeclassInstance env (removeTypesFromConstraint constraint)
  typeclass <- case M.lookup typeclassName (tceClasses env) of
    Just tc -> pure tc
    Nothing -> error "fuck"
  checkInstance env typeclass constraint tcInstance

checkInstance ::
  (MonadError (TCError ann) m, Monoid ann, Ord ann, Show ann) =>
  TCEnv ann ->
  Typeclass ResolvedDep ann ->
  Constraint ResolvedDep (Type ResolvedDep ann) ->
  Instance ResolvedDep ann ->
  m (Instance ResolvedDep (Type ResolvedDep ann))
checkInstance tcEnv typeclass constraint (Instance constraints expr) =
  do
    let subbedType = applyConstraintTypes typeclass constraint

    -- we add the instance's constraints (so typechecker forgives a missing `Eq a` etc)
    let typecheckEnv = tcEnv {tceConstraints = constraints}
        annotatedExpr = EAnn (getExprAnnotation expr) subbedType expr

        typeclassKinds = kindsForTypeclass typeclass
        constraintKinds = kindsForConstraint constraint

    -- now compare these to the constraint

    traceShowM typeclassKinds
    traceShowM constraintKinds

    -- we `elaborate` rather than `typecheck` as we don't want the names
    -- mangled
    (typedExpr, _newConstraints) <- elaborate typecheckEnv annotatedExpr

    let allConstraints = constraints -- nub (constraints <> newConstraints)
    pure $ Instance (addTypesToConstraint <$> allConstraints) typedExpr

-- | what kinds does the actual type have?
kindsForConstraint :: Constraint ResolvedDep ann -> [Kind]
kindsForConstraint (Constraint {conType}) =
  case traverse (typeKind mempty) conType of
    Right yes -> getTypeAnnotation <$> yes
    Left e -> error (show e)

-- | what kinds are expected?
kindsForTypeclass :: Typeclass ResolvedDep ann -> [Kind]
kindsForTypeclass (Typeclass {tcArgs,tcFuncType}) = do
  case typeKind mempty tcFuncType of
    Left e -> error (show e)
    Right tyKind -> catMaybes (lookupKindInType tyKind . emptyResolvedDep <$> tcArgs)
