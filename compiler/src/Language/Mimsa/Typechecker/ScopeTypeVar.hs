{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Typechecker.ScopeTypeVar (freshNamedType) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Store.ExtractTypes
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
    freshen mtV@(MTVar ann (TVName _ tv)) =
      case M.lookup tv known of -- if we've already scoped it
        Nothing -> pure mtV -- leave it
        Just i -> do
          pure (MTVar ann (TVName (Just i) tv))
    freshen mtV@MTVar {} = pure mtV
    freshen mtV@(MTConstructor _ _) =
      pure mtV
    freshen (MTTypeApp ann a b) =
      MTTypeApp ann <$> freshen a <*> freshen b
    freshen (MTPair ann a b) =
      MTPair ann <$> freshen a <*> freshen b
    freshen (MTArray ann as) =
      MTArray ann <$> freshen as
    freshen (MTRecord ann as) =
      MTRecord ann <$> traverse freshen as
    freshen (MTRecordRow ann as a) =
      MTRecordRow ann
        <$> traverse freshen as
        <*> freshen a
    freshen mtP@MTPrim {} = pure mtP
    freshen (MTFunction ann a b) =
      MTFunction ann <$> freshen a <*> freshen b
    freshen (MTContext ann ctx inner) =
      MTContext ann <$> freshen ctx
        <*> freshen inner
