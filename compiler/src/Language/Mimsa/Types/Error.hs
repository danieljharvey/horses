{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error
  ( Error (..),
    InterpreterError (..),
    ResolverError (..),
    TypeErrorF (..),
    TypeError,
    StoreError (..),
    PatternMatchErrorF (..),
    PatternMatchError,
    BackendError (..),
    ProjectError (..),
    FileType (..),
  )
where

import Data.Text (Text)
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.DisplayError
import Language.Mimsa.Types.Error.BackendError
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Error.InterpreterError2
import Language.Mimsa.Types.Error.PatternMatchError
import Language.Mimsa.Types.Error.ProjectError
import Language.Mimsa.Types.Error.ResolverError
import Language.Mimsa.Types.Error.StoreError
import Language.Mimsa.Types.Error.TypeError
import Language.Mimsa.Types.Identifiers

data Error ann
  = TypeErr Text TypeError
  | ResolverErr ResolverError
  | InterpreterErr (InterpreterError ann)
  | InterpreterErr2 (InterpreterError2 Name ann) -- this hardcoded Variable is bad
  | StoreErr StoreError
  | BackendErr (BackendError ann)
  | ProjectErr ProjectError
  | ParseError Text
  deriving stock (Eq, Ord, Show)

instance (Show ann, Printer ann) => Printer (Error ann) where
  prettyPrint (TypeErr input typeErr) = displayError input typeErr
  prettyPrint (ResolverErr a) = "ResolverError:\n" <> prettyPrint a
  prettyPrint (InterpreterErr a) = "InterpreterError:\n" <> prettyPrint a
  prettyPrint (InterpreterErr2 a) = "InterpreterError2:\n" <> prettyPrint a
  prettyPrint (StoreErr a) = "StoreError:\n" <> prettyPrint a
  prettyPrint (BackendErr a) = "BackendError:\n" <> prettyPrint a
  prettyPrint (ProjectErr a) = "ProjectError:\n" <> prettyPrint a
  prettyPrint (ParseError a) = a
