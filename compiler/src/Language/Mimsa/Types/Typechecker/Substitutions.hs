{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker.Substitutions
  ( Substitutions (..),
    applySubst,
  )
where

import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.TypeUtils
import Language.Mimsa.Typechecker.FlattenRow
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker.MonoType

---

newtype Substitutions = Substitutions
  { getSubstitutions ::
      Map TypeIdentifier MonoType
  }
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

substLookup :: ann -> Substitutions -> TypeIdentifier -> Maybe (Type ann)
substLookup ann subst i =
  let replaceAnn mt = mt $> ann
   in replaceAnn <$> M.lookup i (getSubstitutions subst)

instance Substitutable (Type ann) where
  applySubst subst ty = case flattenRow ty of
    MTVar ann var ->
      fromMaybe
        (MTVar ann var)
        (substLookup ann subst var)
    other -> mapMonoType (applySubst subst) other

instance (Substitutable a) => Substitutable (Map k a) where
  applySubst subst as = applySubst subst <$> as

instance Substitutable (Expr var MonoType) where
  applySubst subst elabExpr =
    applySubst subst <$> elabExpr
