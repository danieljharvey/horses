{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Project.Usage where

import qualified Data.Aeson as JSON
import Data.OpenApi (ToSchema)
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store.ExprHash

data Usage
  = Transient Name ExprHash
  | Direct Name ExprHash
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

instance Printer Usage where
  prettyPrint (Transient name _) =
    "Transient dependency of "
      <> prettyPrint name
  prettyPrint (Direct name _) =
    "Direct dependency of "
      <> prettyPrint name

----------
