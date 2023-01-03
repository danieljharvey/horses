{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Core.Types.Type.Unique (Unique (..), getExprHash) where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Core.Types.Module.ModuleHash
import Language.Mimsa.Core.Types.Store.ExprHash

data Unique
  = Unique Int
  | Dependency ExprHash
  | ModuleDep ModuleHash
  | Local
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON)

getExprHash :: Unique -> Maybe ExprHash
getExprHash (Dependency hash) = Just hash
getExprHash _ = Nothing
