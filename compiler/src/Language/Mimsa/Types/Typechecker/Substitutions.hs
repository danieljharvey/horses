{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker.Substitutions
  ( Substitutions (..),
    applySubst,
  )
where

import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker.MonoType

---

newtype Substitutions = Substitutions {getSubstitutions :: Map TypeIdentifier MonoType}
  deriving stock (Eq, Ord, Show)

instance Semigroup Substitutions where
  (Substitutions s1) <> (Substitutions s2) =
    Substitutions $ M.union (M.map (applySubst (Substitutions s1)) s2) s1

instance Monoid Substitutions where
  mempty = Substitutions mempty

instance Printer Substitutions where
  prettyPrint (Substitutions s1) = "\n  " <> T.intercalate "\n  " (printRow <$> M.toList s1)
    where
      printRow (var, mt) = prettyPrint var <> ": " <> prettyPrint mt

---

class Substitutable a where
  applySubst :: Substitutions -> a -> a

-- these are tricky to deal with, so flatten them on the way in
flattenRow :: MonoType -> MonoType
flattenRow (MTRecordRow ann as (MTRecordRow _ann' bs rest)) =
  flattenRow (MTRecordRow ann (as <> bs) rest)
flattenRow other = other

substLookup :: Substitutions -> TypeIdentifier -> Maybe MonoType
substLookup subst i = M.lookup i (getSubstitutions subst)

instance Substitutable MonoType where
  applySubst subst ty = case flattenRow ty of
    MTVar ann var ->
      fromMaybe
        (MTVar ann var)
        (substLookup subst var)
    MTFunction ann arg res ->
      MTFunction ann (applySubst subst arg) (applySubst subst res)
    MTPair ann a b ->
      MTPair
        ann
        (applySubst subst a)
        (applySubst subst b)
    MTRecord ann a ->
      MTRecord ann (applySubst subst <$> a)
    MTRecordRow ann a rest ->
      MTRecordRow ann (applySubst subst <$> a) (applySubst subst rest)
    MTArray ann a ->
      MTArray ann (applySubst subst a)
    MTData ann a ty' ->
      MTData ann a (applySubst subst <$> ty')
    MTTypeApp ann func arg ->
      MTTypeApp ann (applySubst subst func) (applySubst subst arg)
    MTConstructor ann cn -> MTConstructor ann cn
    MTPrim ann a -> MTPrim ann a

instance Substitutable (Expr Variable (MonoType, Annotation)) where
  applySubst subst elabExpr =
    first (applySubst subst) <$> elabExpr
