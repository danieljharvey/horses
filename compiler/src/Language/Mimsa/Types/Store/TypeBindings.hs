{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Store.TypeBindings where

import qualified Data.Aeson as JSON
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Language.Mimsa.Core
import Language.Mimsa.Types.Store.ExprHash (ExprHash)

-- a list of names to hashes
newtype TypeBindings = TypeBindings
  { getTypeBindings ::
      Map TyCon ExprHash
  }
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
