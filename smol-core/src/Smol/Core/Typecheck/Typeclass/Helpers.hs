{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Smol.Core.Typecheck.Typeclass.Helpers
  ( recoverTypeclassUses,
    lookupTypeclassConstraint,
    lookupTypeclassInstance,
    instanceMatchesType,
  )
where

import Control.Monad (unless, void, zipWithM)
import Control.Monad.Except
import Control.Monad.Identity
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Smol.Core.ExprUtils
import Smol.Core.Typecheck.Substitute
import Smol.Core.Typecheck.Types
import Smol.Core.Types

unresolveType :: Type ResolvedDep ann -> Type Identity ann
unresolveType = mapTypeDep resolve
  where
    resolve (LocalDefinition a) = Identity a
    resolve (UniqueDefinition a _) = Identity a
    resolve (TypeclassCall a _) = Identity a

-- this just chucks types in any order and will break on multi-parameter type
-- classes
recoverTypeclassUses :: (Monoid ann) => [TCWrite ann] -> M.Map (ResolvedDep Identifier) (Constraint ann)
recoverTypeclassUses events =
  let allSubs = filterSubstitutions events
      allTCs = filterTypeclassUses events
      substituteMatch (ident, unknownId) =
        (ident, unresolveType $ substituteMany allSubs (TUnknown mempty unknownId))
      fixTC (identifier, name, matches) =
        (identifier, name, substituteMatch <$> matches)
      toConstraint (identifier, name, fixedMatches) =
        M.singleton identifier (Constraint name (snd <$> fixedMatches))
   in mconcat $ toConstraint . fixTC <$> allTCs

-- thing we're matching, typeclass we're checking
matchType ::
  Type Identity ann ->
  Type Identity ann ->
  Either
    (Type Identity ann, Type Identity ann)
    [Substitution Identity ann]
matchType ty (TVar _ ident) =
  Right [Substitution (SubId ident) ty]
matchType (TTuple _ a as) (TTuple _ b bs) = do
  match <- matchType a b
  matches <- zipWithM matchType (NE.toList as) (NE.toList bs)
  pure (match <> mconcat matches)
matchType a b = Left (a, b)

instanceMatchesType ::
  [Type Identity ann] ->
  [Type Identity ann] ->
  Either
    (Type Identity ann, Type Identity ann)
    [Substitution Identity ann]
instanceMatchesType needleTys haystackTys =
  fmap mconcat $ zipWithM matchType needleTys haystackTys

lookupConcreteInstance :: (Ord ann) => TCEnv ann -> Constraint ann -> Maybe (Instance ann)
lookupConcreteInstance env constraint =
  M.lookup constraint (tceInstances env)

-- | do we have a matching instance? if we're looking for a concrete type and
-- it's not there, explode (ie, there is no `Eq Bool`)
-- or return it
lookupTypeclassInstance ::
  (MonadError (TCError ann) m, Ord ann, Show ann) =>
  TCEnv ann ->
  Constraint ann ->
  m (Instance ann)
lookupTypeclassInstance env constraint@(Constraint name tys) =
  -- first, do we have a concrete instance?
  case lookupConcreteInstance env constraint of
    Just tcInstance -> pure tcInstance
    Nothing -> do
      case mapMaybe
        ( \(Constraint innerName innerTys) ->
            case (innerName == name, instanceMatchesType tys innerTys) of
              (True, Right matches) -> Just (Constraint innerName innerTys, matches)
              _ -> Nothing
        )
        (M.keys (tceInstances env)) of
        -- we deliberately fail if we find more than one matching instance
        [(foundConstraint, subs)] -> do
          -- a) look up main instance
          case lookupConcreteInstance env foundConstraint of
            Just (Instance {inConstraints, inExpr}) -> do
              -- specialise contraints to found types
              let subbedConstraints = substituteConstraint subs <$> inConstraints
              -- see if found types exist
              traverse_ (lookupTypeclassInstance env) subbedConstraints
              -- return new instance
              pure (Instance {inConstraints = subbedConstraints, inExpr})
            Nothing ->
              throwError (TCTypeclassInstanceNotFound name tys)
        [] ->
          throwError (TCTypeclassInstanceNotFound name tys)
        multiple ->
          throwError (TCConflictingTypeclassInstancesFound (fst <$> multiple))

substituteConstraint ::
  [Substitution Identity ann] ->
  Constraint ann ->
  Constraint ann
substituteConstraint subs (Constraint name tys) =
  Constraint name (substituteMany subs <$> tys)

-- | do we have a matching constraint?
-- first look for a concrete instance
-- if one is not there, see if we already have a matching constraint
-- in TCEnv (ie, the function has declared `Eq a`)
lookupTypeclassConstraint ::
  (MonadError (TCError ann) m, Ord ann, Show ann) =>
  TCEnv ann ->
  Constraint ann ->
  m ()
lookupTypeclassConstraint env tch@(Constraint name tys) = do
  -- see if this is a valid instance first?
  _ <-
    void (lookupTypeclassInstance env tch) `catchError` \_ -> do
      -- maybe it's a constraint, look there
      unless
        (elem tch (tceConstraints env))
        (throwError (TCTypeclassInstanceNotFound name tys))
  pure ()
