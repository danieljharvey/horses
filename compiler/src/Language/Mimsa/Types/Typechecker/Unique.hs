{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Typechecker.Unique where

import qualified Data.Aeson as JSON

newtype Unique = Unique Int
  deriving newtype (Eq, Ord, Show, Num, JSON.ToJSON)
