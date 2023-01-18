{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module Smol.Core.Types.Annotated (Annotated (..)) where

data Annotated a ann = Annotated ann a
  deriving stock (Eq, Ord, Show, Functor)
