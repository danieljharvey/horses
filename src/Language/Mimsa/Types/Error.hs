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

data Error a
  = TypeErr (TypeError a)
  | ResolverErr ResolverError
  | InterpreterErr (InterpreterError a)
  | UsageErr UsageError
  | ParseErr Text
  | OtherError Text
  deriving (Eq, Ord, Show)

instance (Show a) => Printer (Error a) where
  prettyPrint (TypeErr t) = "TypeError: " <> prettyPrint t
  prettyPrint (ResolverErr a) = "ResolverError: " <> prettyPrint a
  prettyPrint (InterpreterErr a) = "InterpreterError: " <> prettyPrint a
  prettyPrint (UsageErr a) = "UsageError: " <> prettyPrint a
  prettyPrint (ParseErr a) = "ParseError: " <> a
  prettyPrint (OtherError a) = "OtherError: " <> a
