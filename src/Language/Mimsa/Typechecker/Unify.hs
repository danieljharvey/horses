{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Unify
  ( unify,
  )
where

import Control.Monad.Except
import Control.Monad.State (get, put)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types

freeTypeVars :: MonoType -> S.Set TypeVar
freeTypeVars ty = case ty of
  MTVar var ->
    S.singleton var
  MTFunction t1 t2 ->
    S.union (freeTypeVars t1) (freeTypeVars t2)
  MTPair t1 t2 -> S.union (freeTypeVars t1) (freeTypeVars t2)
  MTSum l r -> S.union (freeTypeVars l) (freeTypeVars r)
  MTRecord as -> foldr S.union mempty (freeTypeVars <$> as)
  MTList as -> freeTypeVars as
  MTString -> S.empty
  MTInt -> S.empty
  MTBool -> S.empty
  MTUnit ->
    S.empty

-- | Creates a fresh unification variable and binds it to the given type
varBind :: TypeVar -> MonoType -> TcMonad Substitutions
varBind var ty
  | ty == MTVar var = pure mempty
  | S.member var (freeTypeVars ty) = do
    throwError $
      FailsOccursCheck var ty
  | otherwise = pure $ Substitutions (M.singleton var ty)

unify :: MonoType -> MonoType -> TcMonad Substitutions
unify a b | a == b = pure mempty
unify (MTFunction l r) (MTFunction l' r') = do
  s1 <- unify l l'
  s2 <- unify (applySubst s1 r) (applySubst s1 r')
  pure (s2 <> s1)
unify (MTPair a b) (MTPair a' b') = do
  s1 <- unify a a'
  s2 <- unify (applySubst s1 b) (applySubst s1 b')
  pure (s2 <> s1)
unify (MTSum a b) (MTSum a' b') = do
  s1 <- unify a a'
  s2 <- unify (applySubst s1 b) (applySubst s1 b')
  pure (s2 <> s1)
unify (MTVar u) t = varBind u t
unify (MTList a) (MTList a') = unify a a'
unify (MTRecord as) (MTRecord bs) = do
  let allKeys = S.toList $ M.keysSet as <> M.keysSet bs
  let getRecordTypes = \k -> do
        tyLeft <- getTypeOrFresh k as
        tyRight <- getTypeOrFresh k bs
        unify tyLeft tyRight
  s <- traverse getRecordTypes allKeys
  pure (foldl (<>) mempty s)
unify t (MTVar u) = varBind u t
unify a b =
  throwError $ UnificationError a b

getTypeOrFresh :: Name -> Map Name MonoType -> TcMonad MonoType
getTypeOrFresh name map' = do
  case M.lookup name map' of
    Just found -> pure found
    _ -> getUnknown

getUnknown :: TcMonad MonoType
getUnknown = do
  nextUniVar <- get
  put (nextUniVar + 1)
  pure (MTVar (TVNumber nextUniVar))
