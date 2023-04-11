{-# LANGUAGE DerivingStrategies #-}

module Calc.Types.Prim
  ( Prim (..),
  )
where

import qualified Prettyprinter as PP

data Prim
  = PInt Integer
  | PBool Bool
  deriving stock (Eq, Ord, Show)

instance PP.Pretty Prim where
  pretty (PInt i) = PP.pretty i
  pretty (PBool b) = PP.pretty b
