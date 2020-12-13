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

-------

-- | Infix operators
data Operator
  = -- | `==` - equality between two items
    Equals
  | -- | `+` - integer addition
    Add
  | -- | `-` - integer subtraction
    Subtract
  | -- | `<>` - string concatenation
    StringConcat
  deriving
    ( Eq,
      Ord,
      Show,
      Generic,
      JSON.FromJSON,
      JSON.ToJSON,
      ToSchema
    )

instance Printer Operator where
  prettyDoc Equals = "=="
  prettyDoc Add = "+"
  prettyDoc Subtract = "-"
  prettyDoc StringConcat = "<>"
