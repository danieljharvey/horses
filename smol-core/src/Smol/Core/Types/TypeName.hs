{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Smol.Core.Types.TypeName
  ( TypeName (..),
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.String
import qualified Data.Text as T
import Smol.Core.Types.Constructor
import qualified Prettyprinter as PP

newtype TypeName = TypeName Constructor
  deriving newtype
    ( Eq,
      Ord,
      Show,
      ToJSON,
      ToJSONKey,
      FromJSON,
      FromJSONKey
    )

instance PP.Pretty TypeName where
  pretty (TypeName n) = PP.pretty n

instance IsString TypeName where
  fromString = TypeName . Constructor . T.pack
