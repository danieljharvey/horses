{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Typechecker.Unify
  ( unify,
    freeTypeVars,
    flattenRow,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

freeTypeVars :: MonoType -> S.Set TypeIdentifier
freeTypeVars ty = case ty of
  MTVar _ var ->
    S.singleton var
  MTFunction _ t1 t2 ->
    S.union (freeTypeVars t1) (freeTypeVars t2)
  MTPair _ t1 t2 -> S.union (freeTypeVars t1) (freeTypeVars t2)
  MTRecord _ as -> foldr S.union mempty (freeTypeVars <$> as)
  MTRecordRow _ as rest ->
    foldr S.union mempty (freeTypeVars <$> as)
      <> freeTypeVars rest
  MTArray _ a -> freeTypeVars a
  MTPrim _ _ -> S.empty
  MTConstructor _ _ -> S.empty
  MTTypeApp _ a b -> freeTypeVars a <> freeTypeVars b
  MTContext _ _ctx inner ->
    freeTypeVars inner -- vars in context aren't free

-- | Creates a fresh unification variable and binds it to the given type
varBind ::
  (MonadReader Swaps m, MonadError TypeError m) =>
  Annotation ->
  TypeIdentifier ->
  MonoType ->
  m Substitutions
varBind ann var ty
  | isNamedVar var =
    case ty of
      (MTVar _ (TVUnificationVar _)) ->
        -- a named variable will unify with a unification variable
        pure (Substitutions (M.singleton var (ty $> ann)))
      (MTVar _ tvVar@(TVName _ _))
        | tvVar == var ->
          -- a named var unifies with itself
          pure (Substitutions (M.singleton var (ty $> ann)))
      _ -> throwError (UnificationError ty (MTVar ann var))
  | typeEquals ty (MTVar mempty var) = pure mempty
  | S.member var (freeTypeVars ty) = do
    swaps <- ask
    throwError $
      FailsOccursCheck swaps var ty
  | otherwise = do
    let ty' = ty $> ann
    pure $ Substitutions (M.singleton var ty')

isNamedVar :: TypeIdentifier -> Bool
isNamedVar (TVName _ _n) = True
isNamedVar _ = False

-- these are tricky to deal with, so flatten them on the way in
flattenRow :: MonoType -> MonoType
flattenRow (MTRecordRow ann as (MTRecordRow ann' bs rest)) =
  flattenRow (MTRecordRow (ann <> ann') (as <> bs) rest)
flattenRow other = other

checkMatching ::
  ( MonadReader Swaps m,
    MonadState TypecheckState m,
    MonadError TypeError m
  ) =>
  Annotation ->
  Annotation ->
  Map Name MonoType ->
  Map Name MonoType ->
  Name ->
  m Substitutions
checkMatching ann ann' as bs k = do
  tyLeft <- getRecordItemType ann k as
  tyRight <- getRecordItemType ann' k bs
  unify tyLeft tyRight

unifyRecords ::
  ( MonadReader Swaps m,
    MonadError TypeError m,
    MonadState TypecheckState m
  ) =>
  (Annotation, Map Name MonoType) ->
  (Annotation, Map Name MonoType) ->
  m Substitutions
unifyRecords (ann, as) (ann', bs) = do
  let diffKeys = S.difference (M.keysSet as) (M.keysSet bs)
  if not $ S.null diffKeys
    then throwError (RecordKeyMismatch diffKeys)
    else do
      let allKeys = S.toList $ M.keysSet as <> M.keysSet bs
      s <- traverse (checkMatching ann ann' as bs) allKeys
      pure (mconcat s)

unifyRecordRows ::
  ( MonadReader Swaps m,
    MonadError TypeError m,
    MonadState TypecheckState m
  ) =>
  (Annotation, Map Name MonoType, MonoType) ->
  (Annotation, Map Name MonoType, MonoType) ->
  m Substitutions
unifyRecordRows (ann, as, restA) (ann', bs, restB) = do
  let matchingKeys = S.intersection (M.keysSet as) (M.keysSet bs)
  s1 <- traverse (checkMatching ann ann' as bs) (S.toList matchingKeys)
  let leftKeys = S.difference (M.keysSet as) matchingKeys
      rightKeys = S.difference (M.keysSet bs) matchingKeys
  let filterMap keys =
        M.filterWithKey (\k _ -> S.member k keys)
  newUnknown <- getUnknown ann
  s2 <- unify (MTRecordRow ann (filterMap leftKeys as) newUnknown) restB
  s3 <- unify (MTRecordRow ann' (filterMap rightKeys bs) newUnknown) restA
  pure (mconcat s1 <> s2 <> s3)

unifyRecordWithRow ::
  ( MonadReader Swaps m,
    MonadError TypeError m,
    MonadState TypecheckState m
  ) =>
  (Annotation, Map Name MonoType) ->
  (Annotation, Map Name MonoType, MonoType) ->
  m Substitutions
unifyRecordWithRow (ann, as) (ann', bs, rest) = do
  let rowKeys = M.keysSet bs
  s1 <- traverse (checkMatching ann ann' as bs) (S.toList rowKeys)
  let extraRecordKeys = S.difference (M.keysSet as) rowKeys
      extraRecordMap = M.filterWithKey (\k _ -> S.member k extraRecordKeys) as
  newUnknown <- getUnknown ann'
  s2 <-
    if M.null extraRecordMap
      then pure mempty
      else unify (MTRecordRow ann' extraRecordMap newUnknown) rest
  pure (mconcat s1 <> s2)

unifyPairs ::
  ( MonadReader Swaps m,
    MonadError TypeError m,
    MonadState TypecheckState m
  ) =>
  (MonoType, MonoType) ->
  (MonoType, MonoType) ->
  m Substitutions
unifyPairs (a, b) (a', b') = do
  s1 <- unify a a'
  s2 <- unify (applySubst s1 b) (applySubst s1 b')
  pure (s2 <> s1)

typeEquals :: MonoType -> MonoType -> Bool
typeEquals mtA mtB = (mtA $> ()) == (mtB $> ())

unify ::
  ( MonadReader Swaps m,
    MonadError TypeError m,
    MonadState TypecheckState m
  ) =>
  MonoType ->
  MonoType ->
  m Substitutions
unify tyA tyB =
  case (flattenRow tyA, flattenRow tyB) of
    (a, b)
      | typeEquals a b ->
        pure mempty
    (MTFunction _ l r, MTFunction _ l' r') ->
      unifyPairs (l, r) (l', r')
    (MTPair _ a b, MTPair _ a' b') ->
      unifyPairs (a, b) (a', b')
    (MTRecord ann as, MTRecord ann' bs) ->
      unifyRecords (ann, as) (ann', bs)
    (MTRecordRow ann as restA, MTRecordRow ann' bs restB) ->
      unifyRecordRows (ann, as, restA) (ann', bs, restB)
    (MTRecord ann as, MTRecordRow ann' bs rest) ->
      unifyRecordWithRow (ann, as) (ann', bs, rest)
    (MTRecordRow ann as rest, MTRecord ann' bs) ->
      unifyRecordWithRow (ann', bs) (ann, as, rest)
    (MTTypeApp _ a b, MTTypeApp _ a' b') ->
      unifyPairs (a, b) (a', b')
    (MTArray _ a, MTArray _ b) -> unify a b
    (MTVar ann u, t) -> varBind ann u t
    (t, MTVar ann u) -> varBind ann u t
    (MTContext _ ctxA restA, MTContext _ ctxB restB) ->
      unifyPairs (ctxA, restA) (ctxB, restB)
    (MTContext _ _ctxA rest, b) ->
      unify rest b
    (a, MTContext _ _ctxB rest) ->
      unify a rest
    (a, b) ->
      throwError $ UnificationError a b

getRecordItemType ::
  (MonadError TypeError m) =>
  Annotation ->
  Name ->
  Map Name MonoType ->
  m MonoType
getRecordItemType ann name map' =
  case M.lookup name map' of
    Just found -> pure found
    _ -> throwError (MissingRecordTypeMember ann name map')
