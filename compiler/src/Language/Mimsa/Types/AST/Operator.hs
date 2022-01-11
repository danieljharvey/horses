{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Operator
  ( Operator (..),
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi
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
  | ArrayConcat
  | GreaterThan
  | GreaterThanOrEqualTo
  | LessThan
  | LessThanOrEqualTo
  | Custom InfixOp
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON)

-- | Before custom operators were added the JSON representation was just a
-- string, so we need to fallback to a manual decoder for these older operators
-- where they are found
instance JSON.FromJSON Operator where
  parseJSON =
    JSON.genericParseJSON JSON.defaultOptions

instance ToSchema Operator where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance Printer Operator where
  prettyDoc Equals = "=="
  prettyDoc Add = "+"
  prettyDoc Subtract = "-"
  prettyDoc StringConcat = "++"
  prettyDoc ArrayConcat = "<>"
  prettyDoc GreaterThan = ">"
  prettyDoc GreaterThanOrEqualTo = ">="
  prettyDoc LessThan = "<"
  prettyDoc LessThanOrEqualTo = "<="
  prettyDoc (Custom infixOp) = prettyDoc infixOp
