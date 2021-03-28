{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Unify
  ( unify,
    unifyStrict,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

freeTypeVars :: MonoType -> S.Set TypeIdentifier
freeTypeVars ty = case ty of
  MTVar _ var ->
    S.singleton var
  MTFunction _ t1 t2 ->
    S.union (freeTypeVars t1) (freeTypeVars t2)
  MTPair _ t1 t2 -> S.union (freeTypeVars t1) (freeTypeVars t2)
  MTRecord _ as -> foldr S.union mempty (freeTypeVars <$> as)
  MTArray _ a -> freeTypeVars a
  MTData _ _ as -> foldr S.union mempty (freeTypeVars <$> as)
  MTPrim _ _ -> S.empty

-- | Creates a fresh unification variable and binds it to the given type
varBind :: Annotation -> TypeIdentifier -> MonoType -> TcMonad Substitutions
varBind ann var ty
  | typeEquals ty (MTVar mempty var) = pure mempty
  | S.member var (freeTypeVars ty) = do
    swaps <- ask
    throwError $
      FailsOccursCheck swaps var ty
  | matchPatternMatchLiteral (MTPrim mempty MTString) ty =
    pure (Substitutions (M.singleton var (MTPrim ann MTString)))
  | otherwise = do
    let ty' = ty $> ann
    pure $ Substitutions (M.singleton var ty')

unifyRecords ::
  (Annotation, Map Name MonoType) ->
  (Annotation, Map Name MonoType) ->
  TcMonad Substitutions
unifyRecords (ann, as) (ann', bs) = do
  let allKeys = S.toList $ M.keysSet as <> M.keysSet bs
  let getRecordTypes k = do
        tyLeft <- getRecordItemType ann k as
        tyRight <- getRecordItemType ann' k bs
        unify tyLeft tyRight
  s <- traverse getRecordTypes allKeys
  pure (mconcat s)

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

matchPatternMatchLiteral :: MonoType -> MonoType -> Bool
matchPatternMatchLiteral (MTPrim _ MTString) (MTData _ tyCon _) =
  tyCon == "Str"
matchPatternMatchLiteral (MTArray _ a) (MTData _ tyCon [b]) =
  tyCon == "Arr" && typeEquals a b
matchPatternMatchLiteral _ _ = False

unify :: MonoType -> MonoType -> TcMonad Substitutions
unify tyA tyB =
  case (tyA, tyB) of
    (a, b)
      | typeEquals a b || matchPatternMatchLiteral a b
          || matchPatternMatchLiteral b a ->
        pure mempty
    (MTFunction _ l r, MTFunction _ l' r') ->
      unifyPairs (l, r) (l', r')
    (MTPair _ a b, MTPair _ a' b') -> unifyPairs (a, b) (a', b')
    (MTRecord ann as, MTRecord ann' bs) ->
      unifyRecords (ann, as) (ann', bs)
    (MTData _ a tyAs, MTData _ b tyBs)
      | a == b -> do
        let pairs = zip tyAs tyBs
        s <- traverse (uncurry unify) pairs
        pure (mconcat s)
    (MTVar ann u, t) -> varBind ann u t
    (t, MTVar ann u) -> varBind ann u t
    (a, b) ->
      throwError $ UnificationError a b

-- when combining records in branches of an If we need to check they are
-- actually the same rather than combine them
unifyStrict :: MonoType -> MonoType -> TcMonad Substitutions
unifyStrict tyA tyB =
  case (tyA, tyB) of
    (MTRecord _ as, MTRecord _ bs) -> do
      let diffKeys = S.difference (M.keysSet as) (M.keysSet bs)
      if not $ S.null diffKeys
        then throwError (RecordKeyMismatch diffKeys)
        else unify tyA tyB
    (a, b) -> unify a b

getRecordItemType ::
  Annotation ->
  Name ->
  Map Name MonoType ->
  TcMonad MonoType
getRecordItemType ann name map' =
  case M.lookup name map' of
    Just found -> pure found
    _ -> throwError (MissingRecordTypeMember ann name map')
