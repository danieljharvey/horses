{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- everything used to typecheck typeclasses
module Smol.Core.Typecheck.Typeclass.Typecheck
  ( checkInstance,
    lookupInstanceAndCheck,
  )
where

import Control.Monad.Except
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Maybe
import Smol.Core.Typecheck.Elaborate (elaborate)
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Typeclass.Helpers
import Smol.Core.Typecheck.Typeclass.KindChecker
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

        dataTypes = tceDataTypes tcEnv
        typeclassKinds = kindsForTypeclass dataTypes typeclass
        constraintKinds = kindsForConstraint dataTypes constraint

    liftEither (kindcheckInstance typeclassKinds constraintKinds)

    -- we `elaborate` rather than `typecheck` as we don't want the names
    -- mangled
    (typedExpr, _newConstraints) <- elaborate typecheckEnv annotatedExpr

    let allConstraints = constraints -- nub (constraints <> newConstraints)
    pure $ Instance (addTypesToConstraint <$> allConstraints) typedExpr

-- | check that each item in an instance kind checks
kindcheckInstance :: [(Identifier, Kind)] -> [Kind] -> Either (TCError ann) ()
kindcheckInstance typeclassKinds constraintKinds = do
  let everything = zip typeclassKinds constraintKinds
  let kindcheckPair ((ident, lhsKind), rhsKind) =
        case unifyKinds (fromKind lhsKind) (fromKind rhsKind) of
          Right _ -> pure ()
          Left (KindMismatch a b) ->
            Left (TCTypeclassError $ InstanceKindMismatch ident (toKind a) (toKind b))
          Left kindError ->
            Left (TCKindError kindError)
  traverse_ kindcheckPair everything

-- | what kinds does the actual type have?
kindsForConstraint ::
  M.Map (ResolvedDep TypeName) (DataType ResolvedDep ann) ->
  Constraint ResolvedDep (Type ResolvedDep ann) ->
  [Kind]
kindsForConstraint dataTypes (Constraint {conType}) =
  case traverse (typeKind dataTypes . getTypeAnnotation) conType of
    Right yes -> getTypeAnnotation <$> yes
    Left e -> error (show e)

-- | what kinds are expected?
kindsForTypeclass ::
  M.Map (ResolvedDep TypeName) (DataType ResolvedDep ann) ->
  Typeclass ResolvedDep ann ->
  [(Identifier, Kind)]
kindsForTypeclass dataTypes (Typeclass {tcArgs, tcFuncType}) = do
  case typeKind dataTypes tcFuncType of
    Left e -> error (show e)
    Right tyKind ->
      mapMaybe
        ( \ident ->
            (,) ident
              <$> ( lookupKindInType tyKind
                      . emptyResolvedDep
                      $ ident
                  )
        )
        tcArgs
