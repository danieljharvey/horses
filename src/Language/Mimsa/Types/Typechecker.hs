{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Printer
import Language.Mimsa.Types.Scheme
import Language.Mimsa.Types.Variable

---

newtype Environment = Environment {getEnvironment :: Map Variable Scheme}
  deriving (Eq, Ord, Show, Semigroup, Monoid)

instance Printer Environment where
  prettyPrint (Environment items) = "[\n" <> T.intercalate ", \n" (printRow <$> (M.toList items)) <> "\n]"
    where
      printRow (var, scheme) =
        prettyPrint var <> ": " <> prettyPrint scheme

---

newtype Substitutions = Substitutions {getSubstitutions :: Map Variable MonoType}
  deriving (Eq, Ord, Show)

instance Semigroup Substitutions where
  (Substitutions s1) <> (Substitutions s2) =
    Substitutions $ M.union (M.map (applySubst (Substitutions s1)) s2) s1

instance Monoid Substitutions where
  mempty = Substitutions mempty

---

substLookup :: Substitutions -> Variable -> Maybe MonoType
substLookup subst i = M.lookup i (getSubstitutions subst)

applySubst :: Substitutions -> MonoType -> MonoType
applySubst subst ty = case ty of
  MTVar i ->
    fromMaybe
      (MTVar i)
      (substLookup subst i)
  MTFunction arg res ->
    MTFunction (applySubst subst arg) (applySubst subst res)
  MTPair a b ->
    MTPair
      (applySubst subst a)
      (applySubst subst b)
  MTList a -> MTList (applySubst subst a)
  MTRecord a -> MTRecord (applySubst subst <$> a)
  MTSum a b -> MTSum (applySubst subst a) (applySubst subst b)
  MTInt -> MTInt
  MTString -> MTString
  MTBool -> MTBool
  MTUnit -> MTUnit
