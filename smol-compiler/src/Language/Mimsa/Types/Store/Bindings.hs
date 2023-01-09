{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Store.Bindings where

import qualified Data.Aeson as JSON
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Mimsa.Core
import Language.Mimsa.Types.Store.ExprHash

-- a list of names to hashes
newtype Bindings = Bindings {getBindings :: Map Name ExprHash}
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Semigroup,
      Monoid,
      JSON.FromJSON,
      JSON.ToJSON
    )

instance Printer Bindings where
  prettyPrint (Bindings b) =
    "{ " <> T.intercalate ", " (prettyPrint <$> M.keys b) <> " }"
