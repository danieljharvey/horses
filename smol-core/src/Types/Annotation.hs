{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Annotation
  ( Annotation (..),
  )
where

import Prettyprinter
import Printer

instance Printer Annotation where
  prettyDoc (Location a b) = "(" <> pretty a <> "," <> pretty b <> ")"

data Annotation = Location Int Int
  deriving stock (Eq, Ord, Show)

-- | bullshit instance
instance Semigroup Annotation where
  a <> _ = a

-- bullshit instance
instance Monoid Annotation where
  mempty = Location 0 0
