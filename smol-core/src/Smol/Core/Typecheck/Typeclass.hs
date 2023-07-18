{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Typecheck.Typeclass (checkInstance, recoverTypeclassUses, lookupTypeclassHead) where

import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map.Strict as M
import Smol.Core.ExprUtils
import Smol.Core.Typecheck.Elaborate (elaborate)
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Substitute
import Smol.Core.Typecheck.Types
import Smol.Core.Types

resolveExpr :: Expr Identity ann -> Expr ResolvedDep ann
resolveExpr = mapExprDep resolve
  where
    resolve (Identity a) = emptyResolvedDep a

unresolveType :: Type ResolvedDep ann -> Type Identity ann
unresolveType = mapTypeDep resolve
  where
    resolve (LocalDefinition a) = Identity a
    resolve (UniqueDefinition a _) = Identity a

checkInstance ::
  (MonadError (TCError ann) m, Ord ann, Show ann, Monoid ann) =>
  Typeclass ann ->
  TypeclassHead ann ->
  Instance ann ->
  m (Identifier, Expr ResolvedDep (Type ResolvedDep ann))
checkInstance (Typeclass _ args funcName ty) (TypeclassHead _ tys) (Instance expr) =
  do
    let subs =
          ( \(ident, tySub) ->
              Substitution (SubId $ Identity ident) tySub
          )
            <$> zip args tys

    let subbedType = substituteMany subs ty

    let tcEnv =
          TCEnv
            { tceVars = mempty,
              tceGlobals = mempty,
              tceDataTypes = mempty,
              tceClasses = mempty,
              tceInstances = mempty
            }

    let annotatedExpr = EAnn (getExprAnnotation expr) subbedType expr

    typedExpr <- elaborate tcEnv (resolveExpr annotatedExpr)

    pure (funcName, typedExpr)

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
lookupTypeclassHead :: (Ord ann) => TCEnv ann -> TypeclassHead ann -> Either (TCError ann) ()
lookupTypeclassHead env tch@(TypeclassHead name tys) =
  case M.lookup tch (tceInstances env) of
    Just _ -> pure ()
    Nothing -> Left (TCTypeclassInstanceNotFound name tys)
