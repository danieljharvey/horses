{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error
  ( Error (..),
    InterpreterError (..),
    ResolverError (..),
    TypeError (..),
    UsageError (..),
  )
where

import Data.Text (Text)
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.DisplayError
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Error.ResolverError
import Language.Mimsa.Types.Error.TypeError
import Language.Mimsa.Types.Error.UsageError

data Error ann
  = TypeErr Text TypeError -- input, error
  | ResolverErr ResolverError
  | InterpreterErr (InterpreterError ann)
  | UsageErr UsageError
  | ParseErr Text
  | OtherError Text
  deriving (Eq, Ord, Show)

instance (Show ann, Printer ann) => Printer (Error ann) where
  prettyPrint (TypeErr input typeErr) = displayError input typeErr
  prettyPrint (ResolverErr a) = "ResolverError:\n" <> prettyPrint a
  prettyPrint (InterpreterErr a) = "InterpreterError:\n" <> prettyPrint a
  prettyPrint (UsageErr a) = "UsageError:\n" <> prettyPrint a
  prettyPrint (ParseErr a) = "ParseError:\n" <> a
  prettyPrint (OtherError a) = "OtherError:\n" <> a
