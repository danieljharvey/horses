{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Typechecker.Unique (Unique (..) ) where

import qualified Data.Aeson as JSON
import GHC.Generics

data Unique hash
  = Unique Int
  | Dependency hash
  | Local
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON)
