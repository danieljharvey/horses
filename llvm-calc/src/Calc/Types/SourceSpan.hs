{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Calc.Types.SourceSpan (SourceSpan (..)) where

import GHC.Generics (Generic)

data SourceSpan = SourceSpan
  { ssRowStart :: Int,
    ssRowEnd :: Int,
    ssColStart :: Int,
    ssColEnd :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)
