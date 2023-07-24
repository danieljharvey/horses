{-# LANGUAGE FlexibleContexts #-}

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
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import Smol.Core.ExprUtils
import Smol.Core.Helpers
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
recoverTypeclassUses :: (Monoid ann) => [TCWrite ann] -> M.Map (ResolvedDep Identifier) (TypeclassHead ann)
recoverTypeclassUses events =
  let allSubs = filterSubstitutions events
      allTCs = filterTypeclassUses events
      substituteMatch (ident, unknownId) =
        (ident, unresolveType $ substituteMany allSubs (TUnknown mempty unknownId))
      fixTC (identifier, name, matches) =
        (identifier, name, substituteMatch <$> matches)
      toTypeclassHead (identifier, name, fixedMatches) =
        M.singleton identifier (TypeclassHead name (snd <$> fixedMatches))
   in mconcat $ toTypeclassHead . fixTC <$> allTCs

-- thing we're matching, typeclass we're checking
matchType ::
  Type Identity ann ->
  Type Identity ann ->
  Either
    (Type Identity ann, Type Identity ann)
    (M.Map Identifier (Type Identity ann))
matchType a (TVar _ (Identity b)) =
  Right (M.singleton b a)
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
    (M.Map Identifier (Type Identity ann))
instanceMatchesType needleTys haystackTys =
  fmap mconcat $ zipWithM matchType needleTys haystackTys

-- | do we have a matching instance? if we're looking for a concrete type and
-- it's not there, explode (ie, there is no `Eq Bool`)
-- or return it
lookupTypeclassInstance ::
  (MonadError (TCError ann) m, Ord ann) =>
  TCEnv ann ->
  TypeclassHead ann ->
  m (Instance ann)
lookupTypeclassInstance env tch@(TypeclassHead name tys) =
  -- first, do we have a concrete instance?
  case M.lookup tch (tceInstances env) of
    Just tcInstance -> pure tcInstance
    Nothing -> do
      case listToMaybe $
        mapMaybe
          ( \(TypeclassHead innerName innerTys) ->
              case (innerName == name, instanceMatchesType tys innerTys) of
                (True, Right matches) -> Just (TypeclassHead innerName innerTys, matches)
                _ -> Nothing
          )
          (M.keys (tceInstances env)) of
        Just (TypeclassHead innerName innerTys, matches) -> do
          -- a) look up main instance
          -- b) look up sub instances using `matches`
          tracePrettyM "matching heads" (innerName, innerTys, matches)
          error "poo"
        Nothing ->
          throwError (TCTypeclassInstanceNotFound name tys)

-- | do we have a matching instance? if we're looking for a concrete type and
-- it's not there, explode (ie, there is no `Eq Bool`)
-- if we're looking up `Eq a` though, raise a constraint.
lookupTypeclassConstraint ::
  (MonadError (TCError ann) m, Ord ann) =>
  TCEnv ann ->
  TypeclassHead ann ->
  m ()
lookupTypeclassConstraint env tch@(TypeclassHead name tys) = do
  -- see if this is a valid instance first?
  _ <-
    void (lookupTypeclassInstance env tch) `catchError` \_ -> do
      -- maybe it's a constraint, look there
      unless
        (elem tch (tceConstraints env))
        (throwError (TCTypeclassInstanceNotFound name tys))
  pure ()
