{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Typechecker.Unique (Unique (..), getExprHash) where

import qualified Data.Aeson as JSON
import Data.Hashable (Hashable)
import GHC.Generics
import Language.Mimsa.Core
import Language.Mimsa.Types.Store.ExprHash

data Unique
  = Unique Int
  | Dependency ExprHash
  | ModuleDep ModuleHash
  | Local
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, Hashable)

getExprHash :: Unique -> Maybe ExprHash
getExprHash (Dependency hash) = Just hash
getExprHash _ = Nothing
