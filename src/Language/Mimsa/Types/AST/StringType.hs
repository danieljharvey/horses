{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.AST.StringType
  ( StringType (..),
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer

newtype StringType = StringType Text
  deriving newtype (Eq, Ord, Show, JSON.FromJSON, JSON.ToJSON)
  deriving (Generic, ToSchema)

instance Printer StringType where
  prettyDoc = renderStringType

renderStringType :: StringType -> Doc ann
renderStringType (StringType s) = pretty s
