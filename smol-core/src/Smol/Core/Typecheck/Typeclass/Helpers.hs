{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Typecheck.Typeclass.Helpers
  ( recoverTypeclassUses,
    lookupTypeclassConstraint,
    lookupTypeclassInstance,
  )
where

import Control.Monad (unless, void)
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map.Strict as M
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
recoverTypeclassUses :: (Monoid ann) => [TCWrite ann] -> M.Map (ResolvedDep Identifier) (TypeclassHead ann)
recoverTypeclassUses events =
  let allSubs = filterSubstitutions events
      allTCs = filterTypeclassUses events
      substituteMatch (ident, unknownId) = (ident, unresolveType $ substituteMany allSubs (TUnknown mempty unknownId))
      fixTC (identifier, name, matches) = (identifier, name, substituteMatch <$> matches)
      toTypeclassHead (identifier, name, fixedMatches) =
        M.singleton identifier (TypeclassHead name (snd <$> fixedMatches))
   in mconcat $ toTypeclassHead . fixTC <$> allTCs

-- | do we have a matching instance? if we're looking for a concrete type and
-- it's not there, explode (ie, there is no `Eq Bool`)
-- or return it
lookupTypeclassInstance ::
  (MonadError (TCError ann) m, Ord ann) =>
  TCEnv ann ->
  TypeclassHead ann ->
  m (Instance ann)
lookupTypeclassInstance env tch@(TypeclassHead name tys) =
  case M.lookup tch (tceInstances env) of
    Just tcInstance -> pure tcInstance
    Nothing -> throwError (TCTypeclassInstanceNotFound name tys)

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
