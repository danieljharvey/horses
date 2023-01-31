{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Core.Typecheck.Substitute
  ( Substitution (..),
    SubstitutionMatcher (..),
    substituteMany,
  )
where

import Smol.Core.Types

-- | will need to become `dep Identifier`
data SubstitutionMatcher dep ann
  = SubId Identifier
  | SubUnknown Integer
  | SubType (Type dep ann)

deriving stock instance (Eq ann) => Eq (SubstitutionMatcher dep ann)

deriving stock instance (Ord ann) => Ord (SubstitutionMatcher dep ann)

deriving stock instance (Show ann) => Show (SubstitutionMatcher dep ann)

data Substitution dep ann
  = Substitution (SubstitutionMatcher dep ann) (Type dep ann)
  deriving stock (Eq, Ord, Show)

getSubId :: SubstitutionMatcher dep ann -> Maybe Identifier
getSubId (SubId subId) = Just subId
getSubId _ = Nothing

getUnknownId :: SubstitutionMatcher dep ann -> Maybe Integer
getUnknownId (SubUnknown i) = Just i
getUnknownId _ = Nothing

substituteMany :: [Substitution dep ann] -> Type dep ann -> Type dep ann
substituteMany subs ty =
  foldl (flip substitute) ty subs

substitute :: Substitution dep ann -> Type dep ann -> Type dep ann
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
