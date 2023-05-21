{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Typecheck.Subtype
  ( isSubtypeOf,
    combineMany,
    combine,
    combineTypeMaps,
    generaliseLiteral,
    isNatLiteral,
    isIntLiteral,
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
import qualified Data.Set.NonEmpty as NES
import Smol.Core.Helpers
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Substitute
import Smol.Core.Typecheck.Types
import Smol.Core.Types

combineTypeMaps ::
  ( Eq ann,
    Show ann,
    MonadError (TCError ann) m,
    MonadWriter [Substitution ResolvedDep ann] m
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

-- given a number literal, get the smallest non-literal
generaliseLiteral ::
  ResolvedType ann ->
  ResolvedType ann
generaliseLiteral (TLiteral ann (TLInt tlA)) =
  if all (>= 0) tlA
    then TPrim ann TPNat
    else TPrim ann TPInt
generaliseLiteral (TLiteral ann (TLBool _)) =
  TPrim ann TPBool
generaliseLiteral a = a

combineMany ::
  ( MonadError (TCError ann) m,
    MonadWriter [Substitution ResolvedDep ann] m,
    Show ann,
    Eq ann
  ) =>
  NE.NonEmpty (ResolvedType ann) ->
  m (ResolvedType ann)
combineMany types =
  foldrM combine (NE.head types) (NE.tail types)

-- try and combine two types, either by getting the subtype or union-ing
-- literals together
combine ::
  ( Eq ann,
    Show ann,
    MonadError (TCError ann) m,
    MonadWriter [Substitution ResolvedDep ann] m
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
isLiteralSubtypeOf (TLiteral _ (TLInt a)) (TPrim _ TPNat) = all (>= 0) a -- a Int literal is a Nat if its non-negative
isLiteralSubtypeOf (TLiteral _ (TLInt _)) (TPrim _ TPInt) = True -- a Nat literal is also an Int
isLiteralSubtypeOf (TPrim _ TPNat) (TPrim _ TPInt) = True -- a Nat is also an Int
isLiteralSubtypeOf union (TPrim _ TPNat) | isNatLiteral union = True
isLiteralSubtypeOf union (TPrim _ TPInt) | isIntLiteral union = True
isLiteralSubtypeOf _ _ = False

-- | this is a sign we're encoding unions all wrong I think, but let's just
-- follow this through
isNatLiteral :: Type dep ann -> Bool
isNatLiteral (TLiteral _ (TLInt a)) | all (>= 0) a = True
isNatLiteral _ = False

isIntLiteral :: Type dep ann -> Bool
isIntLiteral (TLiteral _ (TLInt _)) = True
isIntLiteral _ = False

-- smash two types together, learn something
-- Repeat after me, Duck is a subtype of Bird
-- 1 is a subtype of 1 | 2
-- 1 | 2 is a subtype of Nat
-- Nat is a subtype of Int
isSubtypeOf ::
  ( MonadWriter [Substitution ResolvedDep ann] m,
    MonadError (TCError ann) m,
    Eq ann,
    Show ann
  ) =>
  ResolvedType ann ->
  ResolvedType ann ->
  m (ResolvedType ann)
isSubtypeOf a b | isLiteralSubtypeOf a b = pure b -- choose the more general of the two types
isSubtypeOf (TGlobals annA globsA restA) (TGlobals _annB globsB restB) = do
  tyRest <- isSubtypeOf restA restB
  (GlobalMap allGlobs) <- combineTypeMaps (GlobalMap globsA) (GlobalMap globsB)
  pure (TGlobals annA allGlobs tyRest)
isSubtypeOf (TGlobals annA globsA restA) b =
  isSubtypeOf (TGlobals annA globsA restA) (TGlobals annA globsA b)
isSubtypeOf a (TGlobals annB globsB restB) =
  isSubtypeOf (TGlobals annB globsB a) (TGlobals annB globsB restB)
isSubtypeOf (TRecord annA itemsA) (TRecord _annB itemsB) =
  let missing = M.difference itemsB itemsA
   in if M.null missing
        then do
          (GlobalMap allItems) <- combineTypeMaps (GlobalMap itemsA) (GlobalMap itemsB)
          pure (TRecord annA allItems)
        else throwError (TCRecordMissingItems $ M.keysSet missing)
isSubtypeOf (TVar ann a) (TVar ann' b) =
  if a == b
    then pure (TVar ann a)
    else throwError (TCTypeMismatch (TVar ann a) (TVar ann' b))
-- unknowns go before vars because they are weaker, as such
isSubtypeOf (TUnknown _ i) b =
  tell [Substitution (SubUnknown i) b]
    >> pure b
isSubtypeOf a (TUnknown _ i) =
  tell [Substitution (SubUnknown i) a]
    >> pure a
isSubtypeOf (TVar _ ident) b =
  tell [Substitution (SubId ident) b]
    >> pure b
isSubtypeOf a (TVar _ ident) =
  tell [Substitution (SubId ident) a]
    >> pure a
isSubtypeOf (TTuple annA fstA restA) (TTuple _annB fstB restB) =
  do
    tyFst <- isSubtypeOf fstA fstB
    tyRest <- neZipWithM isSubtypeOf restA restB
    pure (TTuple annA tyFst tyRest)
isSubtypeOf (TArray ann i a) (TArray _ _ b) = do
  inner <- isSubtypeOf a b
  pure (TArray ann i inner) -- should we checking array length?
isSubtypeOf tA@(TApp tyAnn lA lB) tB@(TApp _ rA rB) = do
  -- need to check for variables in here
  let result =
        first
          TCExpectedConstructorType
          ((,) <$> flattenConstructorType tA <*> flattenConstructorType tB)
  case result of
    Right ((typeNameA, argsA), (typeNameB, argsB)) -> do
      when (typeNameA /= typeNameB) $ throwError (TCTypeMismatch tA tB)
      tyArgs <- zipWithM isSubtypeOf argsA argsB
      let ann = getTypeAnnotation tA
      pure $ foldl' (TApp ann) (TConstructor ann typeNameA) tyArgs
    Left _ -> do
      -- this might be type vars instead of a concrete TConstructor etc,
      -- so just split and subtype as normal
      tyA <- isSubtypeOf lA rA
      tyB <- isSubtypeOf lB rB
      pure (TApp tyAnn tyA tyB)
isSubtypeOf (TFunc ann lClosure lA lB) (TFunc _ rClosure rA rB) = do
  tyA <- isSubtypeOf lA rA
  tyB <- isSubtypeOf rB lB
  pure (TFunc ann (lClosure <> rClosure) tyA tyB)
isSubtypeOf a b =
  if (a $> ()) == (b $> ())
    then pure a
    else throwError (TCTypeMismatch a b)
