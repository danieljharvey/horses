{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error where

import Data.Text (Text)
import Language.Mimsa.Types.InterpreterError
import Language.Mimsa.Types.Printer
import Language.Mimsa.Types.ResolverError
import Language.Mimsa.Types.TypeError
import Language.Mimsa.Types.Usage

data Error
  = TypeErr TypeError
  | ResolverErr ResolverError
  | InterpreterErr InterpreterError
  | UsageErr UsageError
  | ParseErr Text
  | OtherError Text
  deriving (Eq, Ord, Show)

instance Printer Error where
  prettyPrint (TypeErr t) = "TypeError: " <> prettyPrint t
  prettyPrint (ResolverErr a) = "ResolverError: " <> prettyPrint a
  prettyPrint (InterpreterErr a) = "InterpreterError: " <> prettyPrint a
  prettyPrint (UsageErr a) = "UsageError: " <> prettyPrint a
  prettyPrint (ParseErr a) = "ParseError: " <> a
  prettyPrint (OtherError a) = "OtherError: " <> a
