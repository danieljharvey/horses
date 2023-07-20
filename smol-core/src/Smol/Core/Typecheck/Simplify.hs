{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Typecheck.Simplify (simplifyType) where

import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import Smol.Core.TypeUtils
import Smol.Core.Typecheck.Shared
import Smol.Core.Types

-- reduce infix into result if possible, if not, leave as infix
simplifyAdd ::
  ann ->
  Type dep ann ->
  Type dep ann ->
  Type dep ann
simplifyAdd ann (TLiteral _ (TLInt as)) (TLiteral _ (TLInt bs)) =
  TLiteral ann (TLInt allLiterals)
  where
    allLiterals =
      NES.unsafeFromSet $
        S.map (uncurry (+)) $
          S.cartesianProduct (NES.toSet as) (NES.toSet bs)
simplifyAdd ann (TLiteral _ (TLString as)) (TLiteral _ (TLString bs)) =
  TLiteral ann (TLString allLiterals)
  where
    allLiterals =
      NES.unsafeFromSet $
        S.map (uncurry (<>)) $
          S.cartesianProduct (NES.toSet as) (NES.toSet bs)
simplifyAdd ann (TPrim _ primA) (TPrim _ primB) | primA == primB = TPrim ann primA -- collapse matching prims
simplifyAdd ann (TLiteral _ (TLString _)) (TPrim _ TPString) = TPrim ann TPString
simplifyAdd ann (TPrim _ TPString) (TLiteral _ (TLString _)) = TPrim ann TPString
simplifyAdd ann a (TPrim _ TPInt) | isIntLiteral a = TPrim ann TPInt
simplifyAdd ann (TPrim _ TPInt) b | isIntLiteral b = TPrim ann TPInt
simplifyAdd ann a b = TInfix ann OpAdd a b

simplifyEquals :: ann -> Type dep ann -> Type dep ann -> Type dep ann
simplifyEquals ann (TLiteral _ litA) (TLiteral _ litB) =
  TLiteral ann (TLBool $ litA == litB)
simplifyEquals ann _ _ = TPrim ann TPBool

simplifyType ::
  ( Show (dep Identifier),
    Show (dep TypeName),
    Show ann
  ) =>
  Type dep ann ->
  Type dep ann
simplifyType (TInfix ann OpAdd a b) =
  simplifyAdd ann (simplifyType a) (simplifyType b)
simplifyType (TInfix ann OpEquals a b) =
  simplifyEquals ann (simplifyType a) (simplifyType b)
simplifyType other =
  mapType simplifyType other
