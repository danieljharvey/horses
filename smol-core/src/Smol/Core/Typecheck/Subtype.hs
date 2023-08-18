{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Typecheck.Subtype
  ( isSubtypeOf,
    combineMany,
    combineTypeMaps,
    generaliseLiteral,
    isNatLiteral,
    isIntLiteral,
    typeAddition,
  )
where

import Control.Monad (when, zipWithM)
import Control.Monad.Except
import Control.Monad.Writer.CPS
import Data.Bifunctor (first)
import Data.Foldable (foldl', foldrM)
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import Smol.Core.Helpers
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Simplify
import Smol.Core.Typecheck.Substitute
import Smol.Core.Typecheck.Types
import Smol.Core.Types

combineTypeMaps ::
  ( Eq ann,
    Show ann,
    MonadError (TCError ann) m,
    MonadWriter [TCWrite ann] m
  ) =>
  GlobalMap ann ->
  GlobalMap ann ->
  m (GlobalMap ann)
combineTypeMaps (GlobalMap mapA) (GlobalMap mapB) = do
  let combineTypes (a, b) =
        -- this should probably use combine also
        isSubtypeOf a b
  mapBoth <- traverse combineTypes (M.intersectionWith (,) mapA mapB)
  pure $ GlobalMap (mapBoth <> mapA <> mapB)

-- given a literal, get the "rough" type of it
generaliseLiteral ::
  ResolvedType ann ->
  ResolvedType ann
generaliseLiteral (TLiteral ann (TLInt _)) =
  TPrim ann TPInt
generaliseLiteral (TLiteral ann (TLBool _)) =
  TPrim ann TPBool
generaliseLiteral (TLiteral ann (TLString _)) =
  TPrim ann TPString
generaliseLiteral a = a

-- | used to combine branches of if or case matches
combineMany ::
  ( MonadError (TCError ann) m,
    MonadWriter [TCWrite ann] m,
    Show ann,
    Eq ann
  ) =>
  NE.NonEmpty (ResolvedType ann) ->
  m (ResolvedType ann)
combineMany types =
  foldrM combine (NE.head types) (NE.tail types)

-- | when calculating type for addition, we try and do the actual sum,
-- otherwise treat literals as their more generic types (int, nat, etc) and
-- then subtype as usual to check for errors
typeAddition ::
  ( Eq ann,
    Show ann,
    MonadError (TCError ann) m,
    MonadWriter [TCWrite ann] m
  ) =>
  ResolvedType ann ->
  ResolvedType ann ->
  m (ResolvedType ann)
typeAddition (TLiteral ann (TLInt as)) (TLiteral _ (TLInt bs)) =
  pure $ TLiteral ann (TLInt allLiterals)
  where
    allLiterals =
      NES.unsafeFromSet $
        S.map (uncurry (+)) $
          S.cartesianProduct (NES.toSet as) (NES.toSet bs)
typeAddition (TLiteral ann (TLString as)) (TLiteral _ (TLString bs)) =
  pure $ TLiteral ann (TLString allLiterals)
  where
    allLiterals =
      NES.unsafeFromSet $
        S.map (uncurry (<>)) $
          S.cartesianProduct (NES.toSet as) (NES.toSet bs)
typeAddition (TLiteral ann (TLString _)) b =
  isSubtypeOf (TPrim ann TPString) b
typeAddition a (TLiteral ann (TLString _)) =
  isSubtypeOf a (TPrim ann TPString)
typeAddition a b
  | isIntLiteral a =
      isSubtypeOf (TPrim (getTypeAnnotation a) TPInt) b
typeAddition a b
  | isIntLiteral b =
      isSubtypeOf a (TPrim (getTypeAnnotation b) TPInt)
typeAddition a b =
  isSubtypeOf a b `catchError` const (isSubtypeOf b a)

-- try and combine two types, either by getting the subtype or union-ing
-- literals together
combine ::
  ( Eq ann,
    Show ann,
    MonadError (TCError ann) m,
    MonadWriter [TCWrite ann] m
  ) =>
  ResolvedType ann ->
  ResolvedType ann ->
  m (ResolvedType ann)
combine a b =
  isSubtypeOf a b
    `catchError` const (isSubtypeOf b a)
    `catchError` const asUnion
  where
    asUnion = case (a, b) of
      (TLiteral ann (TLInt as), TLiteral _ (TLInt bs)) ->
        pure $ TLiteral ann (TLInt $ as <> bs)
      (TLiteral ann (TLString as), TLiteral _ (TLString bs)) ->
        pure $ TLiteral ann (TLString $ as <> bs)
      (TLiteral ann (TLBool bA), TLiteral _ (TLBool bB))
        | bA /= bB ->
            pure $ TPrim ann TPBool -- don't have True | False, it's silly
      _ -> throwError (TCTypeMismatch a b)

typeEquals :: ResolvedType ann -> ResolvedType ann -> Bool
typeEquals a b = (a $> ()) == (b $> ())

-- | is the RHS an equal or more general expression of the LHS?
-- | expressed like this so we can try both sides quickly
isLiteralSubtypeOf :: ResolvedType ann -> ResolvedType ann -> Bool
isLiteralSubtypeOf a b | a `typeEquals` b = True
isLiteralSubtypeOf (TLiteral _ (TLBool _)) (TPrim _ TPBool) = True -- a Bool literal is a Bool
isLiteralSubtypeOf (TLiteral _ (TLInt as)) (TLiteral _ (TLInt bs)) | NES.isSubsetOf as bs = True
isLiteralSubtypeOf (TLiteral _ (TLInt _)) (TPrim _ TPInt) = True -- a Nat literal is also an Int
isLiteralSubtypeOf (TLiteral _ (TLString _)) (TPrim _ TPString) = True -- any string literal is a String
isLiteralSubtypeOf union (TPrim _ TPInt) | isIntLiteral union = True
isLiteralSubtypeOf _ _ = False

isSubtypeOf ::
  ( MonadWriter [TCWrite ann] m,
    MonadError (TCError ann) m,
    Eq ann,
    Show ann
  ) =>
  ResolvedType ann ->
  ResolvedType ann ->
  m (ResolvedType ann)
isSubtypeOf a b = isSubtypeInner (simplifyType a) (simplifyType b)

-- smash two types together, learn something
-- Repeat after me, Duck is a subtype of Bird
-- 1 is a subtype of 1 | 2
-- 1 | 2 is a subtype of Nat
-- Nat is a subtype of Int
isSubtypeInner ::
  ( MonadWriter [TCWrite ann] m,
    MonadError (TCError ann) m,
    Eq ann,
    Show ann
  ) =>
  ResolvedType ann ->
  ResolvedType ann ->
  m (ResolvedType ann)
isSubtypeInner a b | isLiteralSubtypeOf a b = pure b -- choose the more general of the two types
isSubtypeInner (TRecord annA itemsA) (TRecord _annB itemsB) =
  let missing = M.difference itemsB itemsA
   in if M.null missing
        then do
          (GlobalMap allItems) <- combineTypeMaps (GlobalMap itemsA) (GlobalMap itemsB)
          pure (TRecord annA allItems)
        else throwError (TCRecordMissingItems $ M.keysSet missing)
isSubtypeInner (TVar ann a) (TVar ann' b) =
  if a == b
    then pure (TVar ann a)
    else throwError (TCTypeMismatch (TVar ann a) (TVar ann' b))
-- unknowns go before vars because they are weaker, as such
isSubtypeInner (TUnknown _ i) b =
  tell [TCWSubstitution $ Substitution (SubUnknown i) b]
    >> pure b
isSubtypeInner a (TUnknown _ i) =
  tell [TCWSubstitution $ Substitution (SubUnknown i) a]
    >> pure a
isSubtypeInner (TVar _ ident) b =
  tell [TCWSubstitution $ Substitution (SubId ident) b]
    >> pure b
isSubtypeInner a (TVar _ ident) =
  tell [TCWSubstitution $ Substitution (SubId ident) a]
    >> pure a
isSubtypeInner (TInfix ann op a b) c =
  TInfix ann op <$> isSubtypeInner a c <*> isSubtypeInner b c
isSubtypeInner (TTuple annA fstA restA) (TTuple _annB fstB restB) =
  do
    tyFst <- isSubtypeInner fstA fstB
    tyRest <- neZipWithM isSubtypeInner restA restB
    pure (TTuple annA tyFst tyRest)
isSubtypeInner (TArray ann i a) (TArray _ _ b) = do
  inner <- isSubtypeInner a b
  pure (TArray ann i inner) -- should we checking array length?
isSubtypeInner tA@(TApp tyAnn lA lB) tB@(TApp _ rA rB) = do
  -- need to check for variables in here
  let result =
        first
          TCExpectedConstructorType
          ((,) <$> flattenConstructorType tA <*> flattenConstructorType tB)
  case result of
    Right ((typeNameA, argsA), (typeNameB, argsB)) -> do
      when (typeNameA /= typeNameB) $ throwError (TCTypeMismatch tA tB)
      tyArgs <- zipWithM isSubtypeInner argsA argsB
      let ann = getTypeAnnotation tA
      pure $ foldl' (TApp ann) (TConstructor ann typeNameA) tyArgs
    Left _ -> do
      -- this might be type vars instead of a concrete TConstructor etc,
      -- so just split and subtype as normal
      tyA <- isSubtypeInner lA rA
      tyB <- isSubtypeInner lB rB
      pure (TApp tyAnn tyA tyB)
isSubtypeInner (TFunc ann lClosure lA lB) (TFunc _ rClosure rA rB) = do
  tyA <- isSubtypeInner lA rA
  tyB <- isSubtypeInner rB lB
  pure (TFunc ann (lClosure <> rClosure) tyA tyB)
isSubtypeInner a b =
  if (a $> ()) == (b $> ())
    then pure a
    else throwError (TCTypeMismatch a b)
