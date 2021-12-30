{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Store.TypeBindings where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import Data.OpenApi
import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Printer (Printer (prettyPrint))
import Language.Mimsa.Types.Identifiers (TyCon, TypeName)
import Language.Mimsa.Types.Store.ExprHash (ExprHash)

-- a list of names to hashes
data TypeBindings = TypeBindings
  { getTypeNameBindings :: Map TypeName ExprHash,
    getTyConBindings :: Map TyCon ExprHash
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON, ToSchema)

instance Semigroup TypeBindings where
  (TypeBindings a b) <> (TypeBindings a' b') =
    TypeBindings (a <> a') (b <> b')

instance Monoid TypeBindings where
  mempty = TypeBindings mempty mempty

instance Printer TypeBindings where
  prettyPrint (TypeBindings tn tc) =
    let prettyKeys =
          (prettyPrint <$> M.keys tn)
            <> (prettyPrint <$> M.keys tc)
     in "{ "
          <> T.intercalate
            ", "
            prettyKeys
          <> " }"
