{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Operator
  ( Operator (..),
  )
where

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
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)

instance ToSchema Operator where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance Printer Operator where
  prettyDoc Equals = "=="
  prettyDoc Add = "+"
  prettyDoc Subtract = "-"
  prettyDoc StringConcat = "<>"
  prettyDoc (Custom infixOp) = prettyDoc infixOp
