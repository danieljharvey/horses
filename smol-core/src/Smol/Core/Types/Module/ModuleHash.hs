{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smol.Core.Types.Module.ModuleHash where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Smol.Core.Printer
import Prettyprinter

-- because of the size of the ints
-- and JS's limitations in the browser
-- we JSON encode these as strings
newtype ModuleHash = ModuleHash Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype
    ( JSON.FromJSON,
      JSON.FromJSONKey,
      JSON.ToJSON,
      JSON.ToJSONKey
    )

instance Show ModuleHash where
  show (ModuleHash a) = T.unpack a

instance Printer ModuleHash where
  prettyDoc (ModuleHash a) = pretty a
