{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Core.Types.NullUnit where

import qualified Data.Aeson as JSON

-- | datatype that encodes to `null` in JSON
data NullUnit = NullUnit
  deriving stock (Eq, Ord, Show)

instance JSON.FromJSON NullUnit where
  parseJSON JSON.Null = pure NullUnit
  parseJSON _ = fail "NullUnit expected a null value"

instance JSON.ToJSON NullUnit where
  toJSON _ = JSON.Null

instance Semigroup NullUnit where
  _ <> _ = NullUnit

instance Monoid NullUnit where
  mempty = NullUnit
