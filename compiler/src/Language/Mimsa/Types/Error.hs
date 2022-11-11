{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wno-orphans #-}

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
    ModuleError (..),
    FileType (..),
    CodegenError (..),
    errorToDiagnostic,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified Error.Diagnose as Diag
import Error.Diagnose.Compat.Megaparsec
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.DisplayError
import Language.Mimsa.Types.AST.Annotation
import Language.Mimsa.Types.Error.BackendError
import Language.Mimsa.Types.Error.CodegenError
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Error.ModuleError
import Language.Mimsa.Types.Error.PatternMatchError
import Language.Mimsa.Types.Error.ProjectError
import Language.Mimsa.Types.Error.ResolverError
import Language.Mimsa.Types.Error.StoreError
import Language.Mimsa.Types.Error.TypeError
import Language.Mimsa.Types.Identifiers
import Text.Megaparsec

instance HasHints Void msg where
  hints _ = mempty

data Error ann
  = TypeErr Text TypeError
  | ResolverErr ResolverError
  | InterpreterErr (InterpreterError Name ann) -- this hardcoded Variable is bad
  | StoreErr StoreError
  | BackendErr (BackendError ann)
  | ProjectErr ProjectError
  | CodegenErr CodegenError
  | ModuleErr ModuleError
  | ParseError Text (ParseErrorBundle Text Void)
  deriving stock (Eq, Show)

instance (Show ann, Printer ann) => Printer (Error ann) where
  prettyPrint (TypeErr input typeErr) = displayError input typeErr
  prettyPrint (ResolverErr a) = prettyPrint a
  prettyPrint (InterpreterErr a) = prettyPrint a
  prettyPrint (StoreErr a) = prettyPrint a
  prettyPrint (BackendErr a) = prettyPrint a
  prettyPrint (ProjectErr a) = prettyPrint a
  prettyPrint (CodegenErr a) = prettyPrint a
  prettyPrint (ModuleErr a) = prettyPrint a
  prettyPrint (ParseError _input errorBundle) = T.pack (errorBundlePretty errorBundle)

errorToDiagnostic :: Error Annotation -> Diag.Diagnostic Text
errorToDiagnostic (ParseError input bundle) =
  let filename = "<repl>"
      diag = errorDiagnosticFromBundle Nothing "Parse error on input" Nothing bundle
   in --   Creates a new diagnostic with no default hints from the bundle returned by megaparsec
      Diag.addFile diag filename (T.unpack input)
errorToDiagnostic (TypeErr input typeErr) =
  typeErrorDiagnostic input typeErr
errorToDiagnostic (ModuleErr modErr) =
  moduleErrorDiagnostic modErr
errorToDiagnostic e =
  let report =
        Diag.err
          Nothing
          (prettyPrint e)
          []
          []
   in Diag.addReport Diag.def report
