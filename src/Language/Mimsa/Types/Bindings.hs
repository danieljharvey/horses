{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Bindings where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Printer

-- a list of names to hashes
newtype Bindings = Bindings {getBindings :: Map Name ExprHash}
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid, JSON.FromJSON, JSON.ToJSON)

instance Printer Bindings where
  prettyPrint (Bindings b) = "{ " <> T.intercalate ", " (prettyPrint <$> M.keys b) <> " }"
