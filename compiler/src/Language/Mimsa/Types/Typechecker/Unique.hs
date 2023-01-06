{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
  {-# LANGUAGE OverloadedStrings #-}
module Language.Mimsa.Types.Typechecker.Unique (Unique (..), getExprHash) where

import Prettyprinter
import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Core
import Language.Mimsa.Types.Store.ExprHash

data Unique
  = Unique Int
  | Dependency ExprHash
  | ModuleDep ModuleHash
  | Local
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON)

instance Printer Unique where
  prettyDoc (Unique i) = pretty i
  prettyDoc (Dependency hash) = prettyDoc hash
  prettyDoc (ModuleDep modHash) = "module " <> prettyDoc modHash
  prettyDoc Local = "local"

getExprHash :: Unique -> Maybe ExprHash
getExprHash (Dependency hash) = Just hash
getExprHash _ = Nothing
