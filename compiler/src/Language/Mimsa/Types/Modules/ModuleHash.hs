{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Modules.ModuleHash where

import qualified Data.Aeson as JSON
import Data.OpenApi
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Printer
import Servant.API

-- because of the size of the ints
-- and JS's limitations in the browser
-- we JSON encode these as strings
newtype ModuleHash = ModuleHash Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype (ToParamSchema, ToSchema)
  deriving newtype
    ( JSON.FromJSON,
      JSON.FromJSONKey,
      JSON.ToJSON,
      JSON.ToJSONKey,
      FromHttpApiData
    )

instance Show ModuleHash where
  show (ModuleHash a) = T.unpack a

instance Printer ModuleHash where
  prettyPrint (ModuleHash a) = a
