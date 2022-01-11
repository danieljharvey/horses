{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Operator
  ( Operator (..),
  )
where

import qualified Data.Aeson as JSON
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
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

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
