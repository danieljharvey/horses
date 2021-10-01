{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.StringPart
  ( StringPart (..),
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi hiding (Pattern, items, name)
import GHC.Generics
import Language.Mimsa.Printer

data StringPart var ann
  = StrWildcard ann
  | StrValue ann var
  deriving stock (Show, Eq, Ord, Functor, Foldable, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

instance
  (ToSchema var, ToSchema ann) =>
  ToSchema (StringPart var ann)
  where
  declareNamedSchema =
    genericDeclareNamedSchema defaultSchemaOptions

instance (Printer var) => Printer (StringPart var ann) where
  prettyDoc (StrWildcard _) = "_"
  prettyDoc (StrValue _ a) = prettyDoc a
