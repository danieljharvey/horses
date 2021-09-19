{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Project.SourceSpan (SourceSpan (..)) where

import qualified Data.Aeson as JSON
import Data.OpenApi
import GHC.Generics

data SourceSpan = SourceSpan
  { ssRowStart :: Int,
    ssRowEnd :: Int,
    ssColStart :: Int,
    ssColEnd :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)
