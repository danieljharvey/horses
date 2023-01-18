{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Smol.Core.Types.ResolvedDep
  ( ResolvedDep (..),
  )
where

import GHC.Generics (Generic)
import Smol.Core.Printer
import Smol.Core.Types.Module.ModuleHash

data ResolvedDep identifier
  = LocalDefinition
      { rdIdentifier :: identifier
      }
  | ModuleDefinition
      { rdIdentifier :: identifier,
        rdModuleHash :: ModuleHash
      }
  | UniqueDefinition
      { rdIdentifier :: identifier,
        rdUnique :: Int
      }
  deriving stock (Eq, Ord, Show, Generic)

instance (Printer identifier) =>
    Printer (ResolvedDep identifier) where
  prettyDoc rd = prettyDoc (rdIdentifier rd)
