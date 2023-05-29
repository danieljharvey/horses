{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Types.Prim
  ( Prim (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import GHC.Natural
import qualified Prettyprinter as PP
import Smol.Core.Printer
import Data.Text (Text)

data Prim
  = PUnit
  | PNat Natural
  | PInt Integer
  | PBool Bool
  | PString Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Printer Prim where
  prettyDoc = renderPrim

renderPrim :: Prim -> PP.Doc doc
renderPrim (PNat i) = PP.pretty i
renderPrim (PInt i) = PP.pretty i
renderPrim (PBool True) = "True"
renderPrim (PBool False) = "False"
renderPrim (PString txt) = PP.pretty txt
renderPrim PUnit = "Unit"
