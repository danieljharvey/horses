{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Typechecker.Types.Unique (Unique (..), getExprHash) where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Core

data Unique hash
  = Unique Int
  | Dependency hash
  | ModuleDep ModuleHash
  | Local
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON)

getExprHash :: Unique hash -> Maybe hash
getExprHash (Dependency hash) = Just hash
getExprHash _ = Nothing
