{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Core.Typecheck.Substitute
  ( Substitution (..),
    SubstitutionMatcher (..),
    substituteMany,
    getUnknownId,
  )
where

import Smol.Core.Typecheck.Types.Substitution
import Smol.Core.Types

getSubId :: SubstitutionMatcher dep ann -> Maybe (dep Identifier)
getSubId (SubId subId) = Just subId
getSubId _ = Nothing

getUnknownId :: SubstitutionMatcher dep ann -> Maybe Integer
getUnknownId (SubUnknown i) = Just i
getUnknownId _ = Nothing

substituteMany :: (Eq (dep Identifier)) => [Substitution dep ann] -> Type dep ann -> Type dep ann
substituteMany subs ty =
  foldl (flip substitute) ty subs

substitute ::
  (Eq (dep Identifier)) =>
  Substitution dep ann ->
  Type dep ann ->
  Type dep ann
substitute sub@(Substitution i ty) = \case
  TVar _ a | Just a == getSubId i -> ty
  TVar ann a -> TVar ann a
  TUnknown _ a | Just a == getUnknownId i -> ty
  TUnknown ann a -> TUnknown ann a
  TConstructor ann a -> TConstructor ann a
  TFunc ann closure fn arg ->
    TFunc ann (substitute sub <$> closure) (substitute sub fn) (substitute sub arg)
  TInfix ann op a b ->
    TInfix ann op (substitute sub a) (substitute sub b)
  TApp ann a b ->
    TApp ann (substitute sub a) (substitute sub b)
  TArray ann size as ->
    TArray ann size (substitute sub as)
  TTuple ann tFst tRest ->
    TTuple ann (substitute sub tFst) (substitute sub <$> tRest)
  TRecord ann items ->
    TRecord ann (fmap (substitute sub) items)
  prim@TPrim {} -> prim
  lit@TLiteral {} -> lit
