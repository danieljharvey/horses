{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.StringPart
  ( StringPart (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Swagger hiding (Pattern, items, name)
import GHC.Generics
import Language.Mimsa.Printer

data StringPart var ann
  = StrWildcard ann
  | StrValue ann var
  deriving (Show, Eq, Ord, Functor, Generic, JSON.FromJSON, JSON.ToJSON)

instance
  (ToSchema var, ToSchema ann) =>
  ToSchema (StringPart var ann)
  where
  declareNamedSchema =
    genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance (Printer var) => Printer (StringPart var ann) where
  prettyDoc (StrWildcard _) = "_"
  prettyDoc (StrValue _ a) = prettyDoc a
