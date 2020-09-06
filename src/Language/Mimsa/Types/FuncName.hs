{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.FuncName where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer

--------

newtype FuncName = FuncName Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, JSON.FromJSON, JSON.ToJSON)

instance Printer FuncName where
  prettyDoc (FuncName a) = pretty a
