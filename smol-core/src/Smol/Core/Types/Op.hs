{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Types.Op
  (
    Op (..),



  )
where

import Control.Monad.Identity
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import Smol.Core.Helpers
import Smol.Core.Printer

data Op = OpAdd | OpEquals
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Printer Op where
  prettyDoc OpAdd = "+"
  prettyDoc OpEquals = "=="

