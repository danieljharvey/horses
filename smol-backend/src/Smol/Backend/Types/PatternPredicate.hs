{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Backend.Types.PatternPredicate
  ( PatternPredicate (..),
  )
where

import Smol.Backend.Types.GetPath

-- TODO: go through pattern, create a big predicate that says "i am matched"
data PatternPredicate p
  = PathEquals GetPath p -- path to value, value it should equal
  | StringEquals GetPath p -- path to value, value it should equal using string equality function
  deriving stock (Eq, Ord, Show)
