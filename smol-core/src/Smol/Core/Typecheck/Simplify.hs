{-# LANGUAGE FlexibleContexts #-}
module Smol.Core.Typecheck.Simplify (simplifyType) where

import Smol.Core.Typecheck.Shared
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import Smol.Core.Types
import Smol.Core.TypeUtils

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
simplifyAdd ann (TLiteral _ (TLString _)) (TPrim _ TPString)   = TPrim ann TPString
simplifyAdd ann (TPrim _ TPString) (TLiteral _ (TLString _)) = TPrim ann TPString
simplifyAdd ann (TPrim _ TPNat) (TPrim _ TPInt) = TPrim ann TPInt
simplifyAdd ann (TPrim _ TPInt) (TPrim _ TPNat) = TPrim ann TPInt
simplifyAdd ann a (TPrim _ TPNat) | isNatLiteral a = TPrim ann TPNat
simplifyAdd ann (TPrim _ TPNat) b | isNatLiteral b = TPrim ann TPNat
simplifyAdd ann a (TPrim _ TPNat) | isIntLiteral a = TPrim ann TPInt
simplifyAdd ann (TPrim _ TPNat) b | isIntLiteral b = TPrim ann TPInt
simplifyAdd ann a (TPrim _ TPInt) | isIntLiteral a = TPrim ann TPInt
simplifyAdd ann (TPrim _ TPInt) b | isIntLiteral b = TPrim ann TPInt
simplifyAdd ann a b = TInfix ann OpAdd a b

simplifyType :: Type dep ann -> Type dep ann
simplifyType (TInfix ann OpAdd a b) =
  simplifyAdd ann a b
simplifyType other =
  mapType simplifyType other
