{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
  {-# LANGUAGE DeriveGeneric #-}

module Smol.Core.Types.Module.Test where

import Smol.Core.Types.Module.TestName
import Smol.Core.Types.Identifier
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data Test
  = UnitTest TestName Identifier
  deriving stock (Eq,Ord,Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

