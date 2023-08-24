{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- everything used to typecheck typeclasses
module Smol.Core.Typecheck.Typeclass.Typecheck
  ( checkInstance,
    lookupInstanceAndCheck,
  )
where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import Smol.Core.Helpers
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

    -- tracePrettyM "checkInstance constraints" constraints
    -- tracePrettyM "checkInstance expr" expr

    -- need to synthesize types for our constraints
    -- tracePrettyM "tceVars" (tceVars tcEnv)

    -- we add the instance's constraints (so typechecker forgives a missing `Eq a` etc)
    let typecheckEnv = tcEnv {tceConstraints = constraints}
        annotatedExpr = EAnn (getExprAnnotation expr) subbedType expr

    -- tracePrettyM "annotatedExpr" annotatedExpr

    -- we `elaborate` rather than `typecheck` as we don't want the names
    -- mangled
    (typedExpr, newConstraints) <- elaborate typecheckEnv annotatedExpr

    tracePrettyM "constraints" constraints
    tracePrettyM "newConstraints" newConstraints

    tracePrettyM "typedExpr" typedExpr

    let allConstraints = constraints -- nub (constraints <> newConstraints)
    pure $ Instance (addTypesToConstraint <$> allConstraints) typedExpr
