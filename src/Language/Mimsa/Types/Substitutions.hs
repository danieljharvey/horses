{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Substitutions where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Variable

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
  MTVar var ->
    fromMaybe
      (MTVar var)
      (substLookup subst var)
  MTFunction arg res ->
    MTFunction (applySubst subst arg) (applySubst subst res)
  MTPair a b ->
    MTPair
      (applySubst subst a)
      (applySubst subst b)
  MTRecord a -> MTRecord (applySubst subst <$> a)
  MTData a ty' -> MTData a (applySubst subst <$> ty')
  MTInt -> MTInt
  MTString -> MTString
  MTBool -> MTBool
  MTUnit -> MTUnit
