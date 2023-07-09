{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Smol.Core.Modules.Types.TestName
  ( TestName (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.String
import Data.Text (Text)
import Smol.Core.Printer

newtype TestName = TestName Text
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)

instance Printer TestName where
  prettyDoc (TestName tn) = prettyDoc tn

instance IsString TestName where
  fromString = TestName . fromString
