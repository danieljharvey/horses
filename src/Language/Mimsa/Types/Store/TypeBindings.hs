{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Store.TypeBindings where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import Data.Swagger
import qualified Data.Text as T
import Language.Mimsa.Printer (Printer (prettyPrint))
import Language.Mimsa.Types.Identifiers (TyCon)
import Language.Mimsa.Types.Store.ExprHash (ExprHash)

-- a list of names to hashes
newtype TypeBindings = TypeBindings {getTypeBindings :: Map TyCon ExprHash}
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Semigroup,
      Monoid,
      JSON.FromJSON,
      JSON.ToJSON,
      ToSchema
    )

instance Printer TypeBindings where
  prettyPrint (TypeBindings b) = "{ " <> T.intercalate ", " (prettyPrint <$> M.keys b) <> " }"
