{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.Info
  ( doInfo,
  )
where

import Data.Text (Text)
import Language.Mimsa.Actions
import Language.Mimsa.Printer
import Language.Mimsa.Repl.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression

----------

doInfo ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  ReplM Annotation ()
doInfo env input expr = do
  (ResolvedExpression type' _ _ _ _) <- liftRepl $ getTypecheckedStoreExpression input env expr
  replPrint $
    prettyPrint expr
      <> "/n:: "
      <> prettyPrint type'

----------
