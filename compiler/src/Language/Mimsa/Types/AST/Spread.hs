{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Spread
  ( Spread (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Swagger hiding (Pattern, items, name)
import GHC.Generics
import Language.Mimsa.Printer

data Spread var ann
  = NoSpread
  | SpreadWildcard ann
  | SpreadValue ann var
  deriving stock
    ( Show,
      Eq,
      Ord,
      Functor,
      Generic
    )
  deriving anyclass
    ( JSON.FromJSON,
      JSON.ToJSON
    )

instance
  (ToSchema ann, ToSchema var) =>
  ToSchema (Spread var ann)
  where
  declareNamedSchema =
    genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance (Printer var, Show var) => Printer (Spread var ann) where
  prettyDoc NoSpread = ""
  prettyDoc (SpreadWildcard _) = ", ..."
  prettyDoc (SpreadValue _ a) = ", ..." <> prettyDoc a
