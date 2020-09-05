{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.FuncName where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import GHC.Generics

--------

newtype FuncName = FuncName Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, JSON.FromJSON, JSON.ToJSON)
