{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error
  ( Error (..),
    InterpreterError (..),
    ResolverError (..),
    TypeError (..),
    StoreError (..),
  )
where

import Data.Text (Text)
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.DisplayError
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Error.ResolverError
import Language.Mimsa.Types.Error.StoreError
import Language.Mimsa.Types.Error.TypeError

data Error ann
  = TypeErr Text TypeError -- input, error
  | ResolverErr ResolverError
  | InterpreterErr (InterpreterError ann)
  | StoreErr StoreError
  | ParseError Text
  deriving (Eq, Ord, Show)

instance (Show ann, Printer ann) => Printer (Error ann) where
  prettyPrint (TypeErr input typeErr) = displayError input typeErr
  prettyPrint (ResolverErr a) = "ResolverError:\n" <> prettyPrint a
  prettyPrint (InterpreterErr a) = "InterpreterError:\n" <> prettyPrint a
  prettyPrint (StoreErr a) = "StoreError:\n" <> prettyPrint a
  prettyPrint (ParseError a) = a
