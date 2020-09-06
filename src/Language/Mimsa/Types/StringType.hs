{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.StringType
  ( StringType (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Language.Mimsa.Printer

newtype StringType = StringType Text
  deriving newtype (Eq, Ord, Show, JSON.FromJSON, JSON.ToJSON)

instance Printer StringType where
  prettyDoc = renderStringType

renderStringType :: StringType -> Doc ann
renderStringType (StringType s) = pretty s
