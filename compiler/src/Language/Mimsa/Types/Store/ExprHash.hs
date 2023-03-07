{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Store.ExprHash where

import qualified Data.Aeson as JSON
import Data.Hashable
import Data.OpenApi
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Core
import Servant.API

-- because of the size of the ints
-- and JS's limitations in the browser
-- we JSON encode these as strings
newtype ExprHash = ExprHash Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype (ToParamSchema, ToSchema)
  deriving newtype
    ( JSON.FromJSON,
      JSON.FromJSONKey,
      JSON.ToJSON,
      JSON.ToJSONKey,
      FromHttpApiData,
      Hashable
    )

instance Show ExprHash where
  show (ExprHash a) = T.unpack a

instance Printer ExprHash where
  prettyPrint (ExprHash a) = a
