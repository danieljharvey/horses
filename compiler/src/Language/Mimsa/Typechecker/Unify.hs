{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Typechecker.Unify
  ( unify,
    freeTypeVars,
    flattenRow,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Typechecker.Generalise
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

-- | Creates a fresh unification variable and binds it to the given type
varBind ::
  (MonadError TypeError m) =>
  Annotation ->
  TypeIdentifier ->
  MonoType ->
  m Substitutions
varBind ann var@(TVName _) _ =
  -- these should always be scoped when found, so if we find them, error
  throwError (UnscopedTypeVarFound ann var)
varBind ann var@(TVVar _ _) mt@(MTVar _ (TVUnificationVar _)) =
  -- numbered vars always combine with unification vars
  pure (Substitutions (M.singleton var (mt $> ann)))
varBind ann var@(TVVar a nameA) mt@(MTVar _ (TVVar b nameB)) =
  -- names and numbers must match to unify
  if nameA == nameB && a == b
    then pure (Substitutions (M.singleton var (mt $> ann)))
    else throwError (UnificationError mt (MTVar ann var))
varBind ann var@(TVVar _ _) mt =
  -- named vars only unify with themselves
  throwError (UnificationError mt (MTVar ann var))
varBind ann var mt
  | typeEquals mt (MTVar mempty var) = pure mempty
  | S.member var (freeTypeVars mt) = do
      throwError $
        FailsOccursCheck var mt
  | otherwise = do
      let mt' = mt $> ann
      pure $ Substitutions (M.singleton var mt')

-- these are tricky to deal with, so flatten them on the way in
flattenRow :: MonoType -> MonoType
flattenRow (MTRecordRow ann as (MTRecordRow _ann' bs rest)) =
  flattenRow (MTRecordRow ann (as <> bs) rest)
flattenRow other = other

checkMatching ::
  ( MonadState TypecheckState m,
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
  ( MonadError TypeError m,
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
  ( MonadError TypeError m,
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
  ( MonadError TypeError m,
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
  ( MonadError TypeError m,
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
  ( MonadError TypeError m,
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
