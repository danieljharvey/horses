{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Typechecker.ScopeTypeVar (freshNamedType) where

import Control.Monad.State
import Data.Coerce
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Store.ExtractTypes
import Language.Mimsa.TypeUtils
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

-- | if we've seen these type vars before, they're the same
-- if not, return fresh versions and a set of things we've now seen
freshNamedType ::
  (MonadState TypecheckState m) =>
  Environment ->
  MonoType ->
  m (Environment, MonoType)
freshNamedType env mt = do
  let currentVars = M.keysSet $ getTypeVarsInScope env
      newNamesSet = extractNamedTypeVars mt
      unseenSet = S.difference newNamesSet currentVars

  unseen <- setToFreshMap unseenSet
  let allSubs = getTypeVarsInScope env <> unseen
  let newEnv = env {getTypeVarsInScope = allSubs}

  newMt <- freshenNamedTypeVars allSubs mt
  pure (newEnv, newMt)

setToFreshMap ::
  (MonadState TypecheckState m, Ord k) =>
  Set k ->
  m (Map k Int)
setToFreshMap keys =
  let emptyMap = M.fromList . fmap (,0 :: Int) . S.toList
   in traverse (const getNextUniVar) (emptyMap keys)

freshenNamedTypeVars ::
  (MonadState TypecheckState m) =>
  Map TyVar Int ->
  Type ann ->
  m (Type ann)
freshenNamedTypeVars known =
  freshen
  where
    freshen (MTVar ann (TVName tv)) = do
      -- get an index for this name, or find an existing one
      case M.lookup tv known of
        Just index ->
          pure (MTVar ann (TVScopedVar index (coerce tv)))
        _ -> error "what?" -- this should have been created above?
    freshen mtV@(MTVar ann (TVScopedVar _ tv)) =
      case M.lookup (coerce tv) known of -- if we've already scoped it
        Nothing -> pure mtV -- leave it
        Just i -> do
          pure (MTVar ann (TVScopedVar i tv))
    freshen other = bindMonoType freshen other
