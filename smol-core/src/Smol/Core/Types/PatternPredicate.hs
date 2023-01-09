{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Types.PatternPredicate
  ( PatternPredicate (..),
  )
where

import Smol.Core.Types.GetPath

-- TODO: go through pattern, create a big predicate that says "i am matched"
data PatternPredicate p
  = PathEquals GetPath p -- path to value, value it should equal
  deriving stock (Eq, Ord, Show)
