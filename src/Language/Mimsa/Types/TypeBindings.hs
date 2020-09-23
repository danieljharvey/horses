{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.TypeBindings where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Printer (Printer (prettyPrint))
import Language.Mimsa.Types.ExprHash (ExprHash)
import Language.Mimsa.Types.Identifiers (TyCon)

-- a list of names to hashes
newtype TypeBindings = TypeBindings {getTypeBindings :: Map TyCon ExprHash}
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
