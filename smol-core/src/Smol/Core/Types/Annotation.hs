{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Types.Annotation
  ( Annotation (..),
  )
where

data Annotation = Location Int Int
  deriving stock (Eq, Ord, Show)

-- | bullshit instance
instance Semigroup Annotation where
  a <> _ = a

-- bullshit instance
instance Monoid Annotation where
  mempty = Location 0 0
