{-# LANGUAGE DeriveFunctor #-}

module Smol.Core.Types.Annotated (Annotated (..)) where

data Annotated a ann = Annotated ann a
  deriving (Eq, Ord, Show, Functor)
