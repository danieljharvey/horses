{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Pattern where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import Data.Swagger hiding (Pattern)
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.Literal
import Language.Mimsa.Types.Identifiers

data Pattern var ann
  = PWildcard ann
  | PVar ann var
  | PLit ann Literal
  | PConstructor ann TyCon [Pattern var ann]
  | PPair
      ann
      (Pattern var ann)
      (Pattern var ann)
  | PRecord
      ann
      (Map Name (Pattern var ann))
  deriving (Show, Eq, Ord, Functor, Generic, JSON.FromJSON, JSON.ToJSON)

instance (ToSchema var, ToSchema ann, JSON.ToJSONKey var) => ToSchema (Pattern var ann) where
  declareNamedSchema =
    genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance Printer (Pattern var ann) where
  prettyDoc _ = ""
