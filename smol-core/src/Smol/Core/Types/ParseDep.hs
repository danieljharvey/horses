{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Smol.Core.Types.ParseDep
  (

    ParseDep (..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Smol.Core.Printer
import Smol.Core.Types.Module.ModuleName

---------------------------

data ParseDep identifier = ParseDep
  { pdIdentifier :: identifier,
    pdModules :: [ModuleName]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance (Printer identifier) => Printer (ParseDep identifier) where
  prettyDoc (ParseDep ident _) = prettyDoc ident -- we'll print modules later


