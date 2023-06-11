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

import qualified Prettyprinter as PP
import Smol.Core.Printer
import Smol.Core.Types

data SubstitutionMatcher dep ann
  = SubId (dep Identifier)
  | SubUnknown Integer
  | SubType (Type dep ann)

deriving stock instance
  ( Eq ann,
    Eq (dep Identifier),
    Eq (dep TypeName)
  ) =>
  Eq (SubstitutionMatcher dep ann)

deriving stock instance
  ( Ord ann,
    Ord (dep Identifier),
    Ord (dep TypeName)
  ) =>
  Ord (SubstitutionMatcher dep ann)

deriving stock instance
  ( Show ann,
    Show (dep Identifier),
    Show (dep TypeName)
  ) =>
  Show (SubstitutionMatcher dep ann)

data Substitution dep ann
  = Substitution (SubstitutionMatcher dep ann) (Type dep ann)

instance (Show ann, Show (dep Identifier), Show (dep TypeName)) => Printer (Substitution dep ann) where
  prettyDoc a = PP.pretty (show a)

deriving stock instance
  (Eq ann, Eq (dep Identifier), Eq (dep TypeName)) =>
  Eq (Substitution dep ann)

deriving stock instance
  (Ord ann, Ord (dep Identifier), Ord (dep TypeName)) =>
  Ord (Substitution dep ann)

deriving stock instance
  (Show ann, Show (dep Identifier), Show (dep TypeName)) =>
  Show (Substitution dep ann)

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
substitute sub = \case
  TGlobals ann tGlobs tBody ->
    TGlobals ann (fmap (substituteLocal sub) tGlobs) (substituteLocal sub tBody)
  TLocal inner -> TLocal $ substituteLocal sub inner

substituteLocal ::
  (Eq (dep Identifier)) =>
  Substitution dep ann ->
  LocalType dep ann ->
  LocalType dep ann
substituteLocal sub@(Substitution i ty) = \case
  TVar _ a | Just a == getSubId i -> ty
  TVar ann a -> TVar ann a
  TUnknown _ a | Just a == getUnknownId i -> ty
  TUnknown ann a -> TUnknown ann a
  TConstructor ann a -> TConstructor ann a
  TFunc ann closure fn arg ->
    TFunc ann (substituteLocal sub <$> closure) (substituteLocal sub fn) (substituteLocal sub arg)
  TApp ann a b ->
    TApp ann (substituteLocal sub a) (substituteLocal sub b)
  TArray ann size as ->
    TArray ann size (substituteLocal sub as)
  TTuple ann tFst tRest ->
    TTuple ann (substituteLocal sub tFst) (substituteLocal sub <$> tRest)
  TGlobals ann tGlobs tBody ->
    TGlobals ann (fmap (substituteLocal sub) tGlobs) (substituteLocal sub tBody)
  TRecord ann items ->
    TRecord ann (fmap (substituteLocal sub) items)
  prim@TPrim {} -> prim
  lit@TLiteral {} -> lit
