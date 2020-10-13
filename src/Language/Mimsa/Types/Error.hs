{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error
  ( Error (..),
    InterpreterError (..),
    ResolverError (..),
    TypeError (..),
  )
where

import Data.Text (Text)
import Language.Mimsa.Printer
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Error.ResolverError
import Language.Mimsa.Types.Error.TypeError
import Language.Mimsa.Types.Usage

data Error ann
  = TypeErr (TypeError ann)
  | ResolverErr ResolverError
  | InterpreterErr (InterpreterError ann)
  | UsageErr UsageError
  | ParseErr Text
  | OtherError Text
  deriving (Eq, Ord, Show)

instance (Show ann, Printer ann) => Printer (Error ann) where
  prettyPrint (TypeErr t) = "TypeError:\n" <> prettyPrint t
  prettyPrint (ResolverErr a) = "ResolverError:\n" <> prettyPrint a
  prettyPrint (InterpreterErr a) = "InterpreterError:\n" <> prettyPrint a
  prettyPrint (UsageErr a) = "UsageError:\n" <> prettyPrint a
  prettyPrint (ParseErr a) = "ParseError:\n" <> a
  prettyPrint (OtherError a) = "OtherError:\n" <> a
