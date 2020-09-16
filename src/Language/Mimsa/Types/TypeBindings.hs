{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.TypeBindings where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.Identifiers

-- a list of names to hashes
newtype TypeBindings = TypeBindings {getTypeBindings :: Map Construct ExprHash}
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Semigroup,
      Monoid,
      JSON.FromJSON,
      JSON.ToJSON
    )

instance Printer TypeBindings where
  prettyPrint (TypeBindings b) = "{ " <> T.intercalate ", " (prettyPrint <$> M.keys b) <> " }"
