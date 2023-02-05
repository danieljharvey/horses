{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Calc.Types.Prim
  ( Prim (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Prettyprinter as PP

newtype Prim
  = PInt Integer
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance PP.Pretty Prim where
  pretty = renderPrim

renderPrim :: Prim -> PP.Doc doc
renderPrim (PInt i) = PP.pretty i
