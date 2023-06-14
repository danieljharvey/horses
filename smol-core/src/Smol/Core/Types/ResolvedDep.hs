{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Smol.Core.Types.ResolvedDep
  ( ResolvedDep (..),
    emptyResolvedDep,
  )
where

import Data.String
import GHC.Generics (Generic)
import Smol.Core.Printer

data ResolvedDep identifier
  = LocalDefinition
      { rdIdentifier :: identifier
      }
  | UniqueDefinition
      { rdIdentifier :: identifier,
        rdUnique :: Int
      }
  deriving stock (Eq, Ord, Show, Generic)

instance
  (Printer identifier) =>
  Printer (ResolvedDep identifier)
  where
  prettyDoc rd = prettyDoc (rdIdentifier rd)

instance (IsString a) => IsString (ResolvedDep a) where
  fromString = emptyResolvedDep . fromString

emptyResolvedDep :: a -> ResolvedDep a
emptyResolvedDep = LocalDefinition
