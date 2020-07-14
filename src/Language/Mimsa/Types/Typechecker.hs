{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Scheme
import Language.Mimsa.Types.Variable

type Environment = Map Variable Scheme

newtype Substitutions = Substitutions {getSubstitutions :: Map Variable MonoType}
  deriving (Eq, Ord, Show)

instance Semigroup Substitutions where
  (Substitutions s1) <> (Substitutions s2) =
    Substitutions $ M.union (M.map (applySubst (Substitutions s1)) s2) s1

instance Monoid Substitutions where
  mempty = Substitutions mempty

applySubst :: Substitutions -> MonoType -> MonoType
applySubst subst ty = case ty of
  MTVar i ->
    fromMaybe (MTVar i) (M.lookup (NamedVar i) (getSubstitutions subst))
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
