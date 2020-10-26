{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Unify
  ( unify,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types

freeTypeVars :: MonoType -> S.Set Variable
freeTypeVars ty = case ty of
  MTVar _ var ->
    S.singleton var
  MTFunction _ t1 t2 ->
    S.union (freeTypeVars t1) (freeTypeVars t2)
  MTPair _ t1 t2 -> S.union (freeTypeVars t1) (freeTypeVars t2)
  MTRecord _ as -> foldr S.union mempty (freeTypeVars <$> as)
  MTData _ _ as -> foldr S.union mempty (freeTypeVars <$> as)
  MTPrim _ _ -> S.empty

-- | Creates a fresh unification variable and binds it to the given type
varBind :: Variable -> MonoType -> TcMonad Substitutions
varBind var ty
  | typeEquals ty (MTVar mempty var) = pure mempty
  | S.member var (freeTypeVars ty) = do
    swaps <- ask
    throwError $
      FailsOccursCheck swaps var ty
  | otherwise =
    pure $ Substitutions (M.singleton var ty)

unifyPairs ::
  (MonoType, MonoType) ->
  (MonoType, MonoType) ->
  TcMonad Substitutions
unifyPairs (a, b) (a', b') = do
  s1 <- unify a a'
  s2 <- unify (applySubst s1 b) (applySubst s1 b')
  pure (s2 <> s1)

typeEquals :: MonoType -> MonoType -> Bool
typeEquals mtA mtB = (mtA $> ()) == (mtB $> ())

unify :: MonoType -> MonoType -> TcMonad Substitutions
unify tyA tyB =
  case (tyA, tyB) of
    (a, b) | typeEquals a b -> pure mempty
    (MTFunction _ l r, MTFunction _ l' r') ->
      unifyPairs (l, r) (l', r')
    (MTPair _ a b, MTPair _ a' b') -> unifyPairs (a, b) (a', b')
    (MTRecord _ as, MTRecord _ bs) -> do
      let allKeys = S.toList $ M.keysSet as <> M.keysSet bs
      let getRecordTypes k = do
            tyLeft <- getTypeOrFresh k as
            tyRight <- getTypeOrFresh k bs
            unify tyLeft tyRight
      s <- traverse getRecordTypes allKeys
      pure (mconcat s)
    (MTData _ a tyAs, MTData _ b tyBs)
      | a == b -> do
        let pairs = zip tyAs tyBs
        s <- traverse (uncurry unify) pairs
        pure (mconcat s)
    (MTVar _ u, t) -> varBind u t
    (t, MTVar _ u) -> varBind u t
    (a, b) ->
      throwError $ UnificationError a b

getTypeOrFresh :: Name -> Map Name MonoType -> TcMonad MonoType
getTypeOrFresh name map' =
  case M.lookup name map' of
    Just found -> pure found
    _ -> getUnknown
