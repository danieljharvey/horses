{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.BackendError where

import Language.Mimsa.Backend.Typescript.Printer
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Core
import Prettyprinter

data BackendError ann
  = CustomOperatorNotFound InfixOp
  | OutputingTypedHole Name
  | ExpectedExprGotBody TSExpr [TSStatement]
  | ExpectedFunctionType TSType
  | PatternMatchIsEmpty
  | NoConstructorInTypeApp
  deriving stock (Eq, Ord, Show, Functor)

instance Printer (BackendError ann) where
  prettyDoc (CustomOperatorNotFound op) =
    "Could not find operator for output:" <+> prettyDoc op
  prettyDoc PatternMatchIsEmpty = "Pattern match is empty"
  prettyDoc (OutputingTypedHole n) =
    "Trying to output a typed hold, which should not pass typechecking: " <> prettyDoc n
  prettyDoc (ExpectedExprGotBody exp' exps) =
    "Expected no extra exprs for :"
      <> pretty (printExpr exp')
      <> ", but found: "
      <> pretty (printStatement <$> exps)
  prettyDoc (ExpectedFunctionType tsType) =
    "Expected function type but got " <> pretty (printType tsType)
  prettyDoc NoConstructorInTypeApp =
    "No constructor found in type application."
