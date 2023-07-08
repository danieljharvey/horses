{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Smol.Core.Types.ParseDep
  ( ParseDep (..),
    emptyParseDep,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.String
import GHC.Generics (Generic)
import Smol.Core.Printer
import Smol.Core.Modules.Types.ModuleName

---------------------------

data ParseDep identifier = ParseDep
  { pdIdentifier :: identifier,
    pdModules :: Maybe ModuleName
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance (Printer identifier) => Printer (ParseDep identifier) where
  prettyDoc (ParseDep ident _) = prettyDoc ident -- we'll print modules later

instance (IsString a) => IsString (ParseDep a) where
  fromString = emptyParseDep . fromString

emptyParseDep :: a -> ParseDep a
emptyParseDep a = ParseDep a Nothing
