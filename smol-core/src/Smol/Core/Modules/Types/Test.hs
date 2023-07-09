{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Smol.Core.Modules.Types.Test where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Smol.Core.Modules.Types.TestName
import Smol.Core.Types.Identifier

data Test
  = UnitTest TestName Identifier
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
