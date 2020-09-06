{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.ExprHash where

import qualified Data.Aeson as JSON

newtype ExprHash = ExprHash Int
  deriving (Eq, Ord, Show)
  deriving newtype (JSON.FromJSON, JSON.ToJSON)
