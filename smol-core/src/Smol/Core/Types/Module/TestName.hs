{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Smol.Core.Types.Module.TestName
  ( TestName (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Smol.Core.Printer

newtype TestName = TestName Text
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)

instance Printer TestName where
 prettyDoc (TestName tn) = prettyDoc tn
