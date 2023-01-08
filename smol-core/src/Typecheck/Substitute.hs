{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Typecheck.Substitute
  ( Substitution (..),
    SubstitutionMatcher (..),
    substituteMany,
  )
where

import Types

data SubstitutionMatcher ann = SubId Identifier | SubUnknown Integer | SubType (Type ann)
  deriving stock (Eq, Ord, Show)

data Substitution ann = Substitution (SubstitutionMatcher ann) (Type ann)
  deriving stock (Eq, Ord, Show)

getSubId :: SubstitutionMatcher ann -> Maybe Identifier
getSubId (SubId subId) = Just subId
getSubId _ = Nothing

getUnknownId :: SubstitutionMatcher ann -> Maybe Integer
getUnknownId (SubUnknown i) = Just i
getUnknownId _ = Nothing

substituteMany :: [Substitution ann] -> Type ann -> Type ann
substituteMany subs ty =
  foldl (flip substitute) ty subs

substitute :: Substitution ann -> Type ann -> Type ann
substitute sub@(Substitution i ty) = \case
  TVar _ a | Just a == getSubId i -> ty
  TVar ann a -> TVar ann a
  TUnknown _ a | Just a == getUnknownId i -> ty
  TUnknown ann a -> TUnknown ann a
  TConstructor ann a -> TConstructor ann a
  TFunc ann closure fn arg ->
    TFunc ann (substitute sub <$> closure) (substitute sub fn) (substitute sub arg)
  TApp ann a b ->
    TApp ann (substitute sub a) (substitute sub b)
  TTuple ann tFst tRest ->
    TTuple ann (substitute sub tFst) (substitute sub <$> tRest)
  TGlobals ann tGlobs tBody ->
    TGlobals ann (fmap (substitute sub) tGlobs) (substitute sub tBody)
  TRecord ann items ->
    TRecord ann (fmap (substitute sub) items)
  TUnion ann a b -> TUnion ann (substitute sub a) (substitute sub b)
  prim@TPrim {} -> prim
  lit@TLiteral {} -> lit
