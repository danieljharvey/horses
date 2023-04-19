{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Types.GetPath
  ( GetPath (..),
    GetValue (..),
  )
where

-- how do we get at a given item that we've matched on
data GetPath = GetPath [Integer] GetValue
  deriving stock (Eq, Ord, Show)

data GetValue
  = GetValue -- fetch the item pointed to here
  | GetArrayTail Integer -- fetch the array here, but drop X items
  deriving stock (Eq, Ord, Show)
