{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Types.Prim
  ( Prim (..),
  )
where

import GHC.Natural
import qualified Prettyprinter as PP
import Smol.Core.Printer

data Prim
  = PUnit
  | PNat Natural
  | PInt Integer
  | PBool Bool
  deriving stock (Eq, Ord, Show)

instance Printer Prim where
  prettyDoc = renderPrim

renderPrim :: Prim -> PP.Doc doc
renderPrim (PNat i) = PP.pretty i
renderPrim (PInt i) = PP.pretty i
renderPrim (PBool True) = "True"
renderPrim (PBool False) = "False"
renderPrim PUnit = "Unit"
