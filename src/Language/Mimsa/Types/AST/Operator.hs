{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Operator
  ( Operator (..),
  )
where

import Control.Applicative
import qualified Data.Aeson as JSON
import Data.Swagger
import GHC.Generics (Generic)
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.InfixOp

-------

-- | Infix operators
data Operator
  = Equals
  | Add
  | Subtract
  | StringConcat
  | Custom InfixOp
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON)

-- | Before custom operators were added the JSON representation was just a
-- string, so we need to fallback to a manual decoder for these older operators
-- where they are found
instance JSON.FromJSON Operator where
  parseJSON input = JSON.genericParseJSON JSON.defaultOptions input
    <|> case input of
      (JSON.String "Equals") -> pure Equals
      (JSON.String "Add") -> pure Add
      (JSON.String "Subtract") -> pure Subtract
      (JSON.String "StringConcat") -> pure StringConcat
      _ -> fail "Could not decode Operator"

instance ToSchema Operator where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance Printer Operator where
  prettyDoc Equals = "=="
  prettyDoc Add = "+"
  prettyDoc Subtract = "-"
  prettyDoc StringConcat = "<>"
  prettyDoc (Custom infixOp) = prettyDoc infixOp
