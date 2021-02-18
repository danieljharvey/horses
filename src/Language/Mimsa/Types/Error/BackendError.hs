{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.BackendError where

import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

data BackendError ann
  = TyConFindError (Expr Name ann)
  | OutputtingCustomOperator InfixOp
  | OutputingTypedHole Name
  deriving (Eq, Ord, Show)

instance Printer (BackendError ann) where
  prettyDoc (TyConFindError expr) =
    "Error finding type constructor in " <> prettyDoc expr
  prettyDoc (OutputtingCustomOperator op) =
    "Trying to output a custom operator, which should have been substituted for a function by now. " <> prettyDoc op
  prettyDoc (OutputingTypedHole n) =
    "Trying to output a typed hold, which should not pass typechecking: " <> prettyDoc n
