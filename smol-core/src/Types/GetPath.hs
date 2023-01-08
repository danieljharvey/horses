{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Types.GetPath
  ( GetPath (..),
  )
where

import qualified Data.List.NonEmpty as NE

-- how do we get at a given item that we've matched on
-- empty struct path doesn't make sense, maybe look to remove
data GetPath
  = StructPath (NE.NonEmpty Integer)
  | ValuePath
  deriving stock (Eq, Ord, Show)
