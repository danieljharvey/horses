{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.Info
  ( doInfo,
  )
where

import Data.Text (Text)
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression

----------

doInfo ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  MimsaM (Error Annotation) ()
doInfo env input expr = do
  (ResolvedExpression type' _ _ _ _ _) <-
    mimsaFromEither $
      Actions.getTypecheckedStoreExpression input env expr
  replOutput $
    prettyPrint expr
      <> "/n:: "
      <> prettyPrint type'

----------
