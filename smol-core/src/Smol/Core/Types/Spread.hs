{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
  {-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE StandaloneDeriving #-}
module Smol.Core.Types.Spread
  ( Spread (..),
  )
where

import Smol.Core.Types.Identifier
import qualified Data.Aeson as JSON
import GHC.Generics
import Smol.Core.Printer

data Spread dep ann
  = NoSpread
  | SpreadWildcard
      { sprAnn :: ann
      }
  | SpreadValue
      { sprAnn :: ann,
        sprVar :: dep Identifier
      }
  deriving stock
    (


      Functor,
      Foldable,
      Generic,
      Traversable
    )

deriving stock instance (Eq ann, Eq (dep Identifier)) =>
  Eq (Spread dep ann)

deriving stock instance (Ord ann, Ord (dep Identifier)) =>
  Ord (Spread dep ann)

deriving stock instance (Show ann, Show (dep Identifier)) =>
  Show (Spread dep ann)

deriving anyclass instance (JSON.FromJSON ann, JSON.FromJSON (dep Identifier))
  => JSON.FromJSON (Spread dep ann)

deriving anyclass instance (JSON.ToJSON ann, JSON.ToJSON (dep Identifier))
  => JSON.ToJSON (Spread dep ann)


instance (Printer (dep Identifier)) => Printer (Spread dep ann) where
  prettyDoc NoSpread = ""
  prettyDoc (SpreadWildcard _) = ", ..."
  prettyDoc (SpreadValue _ a) = ", ..." <> prettyDoc a
