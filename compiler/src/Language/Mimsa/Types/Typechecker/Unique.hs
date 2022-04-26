{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Typechecker.Unique (Unique (..)) where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Types.Store.ExprHash

data Unique
  = Unique Int
  | Dependency ExprHash
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON)
