
{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Typecheck.Typeclass.Helpers ( recoverTypeclassUses, lookupTypeclassHead) where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import Control.Monad.Identity
import Smol.Core.ExprUtils
import Smol.Core.Typecheck.Substitute
import Smol.Core.Typecheck.Types
import Smol.Core.Types

unresolveType :: Type ResolvedDep ann -> Type Identity ann
unresolveType = mapTypeDep resolve
  where
    resolve (LocalDefinition a) = Identity a
    resolve (UniqueDefinition a _) = Identity a

-- this just chucks types in any order and will break on multi-parameter type
-- classes
recoverTypeclassUses :: (Monoid ann) => [TCWrite ann] -> [TypeclassHead ann]
recoverTypeclassUses events =
  let allSubs = filterSubstitutions events
      allTCs = filterTypeclassUses events
      substituteMatch (ident, unknownId) = (ident, unresolveType $ substituteMany allSubs (TUnknown mempty unknownId))
      fixTC (name, matches) = (name, substituteMatch <$> matches)
      toTypeclassHead (name, fixedMatches) =
        TypeclassHead name (snd <$> fixedMatches)
   in toTypeclassHead . fixTC <$> allTCs

-- | do we have a matching instance? explode if not
lookupTypeclassHead :: (MonadError (TCError ann) m, Ord ann) => TCEnv ann -> TypeclassHead ann -> m ()
lookupTypeclassHead env tch@(TypeclassHead name tys)
  = case M.lookup tch (tceInstances env) of
      Just _ -> pure ()
      Nothing -> throwError (TCTypeclassInstanceNotFound name tys)
