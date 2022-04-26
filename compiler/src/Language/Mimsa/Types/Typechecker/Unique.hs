{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Typechecker.Unique (Unique (..), getExprHash) where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Types.Store.ExprHash

data Unique
  = Unique Int
  | Dependency ExprHash
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON)

getExprHash :: Unique -> Maybe ExprHash
getExprHash (Dependency hash) = Just hash
getExprHash _ = Nothing
