{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Substitutions where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.MonoType

---

newtype Substitutions = Substitutions {getSubstitutions :: Map Variable MonoType}
  deriving (Eq, Ord, Show)

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

substLookup :: Substitutions -> Variable -> Maybe MonoType
substLookup subst i = M.lookup i (getSubstitutions subst)

applySubst :: Substitutions -> MonoType -> MonoType
applySubst subst ty = case ty of
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
  MTData ann a ty' ->
    MTData ann a (applySubst subst <$> ty')
  MTPrim ann a -> MTPrim ann a
