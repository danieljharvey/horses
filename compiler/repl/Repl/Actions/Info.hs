{-# LANGUAGE OverloadedStrings #-}

module Repl.Actions.Info
  ( doInfo,
  )
where

import Data.Text (Text)
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
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
doInfo project input expr = do
  (_, _, resolvedExpr) <-
    mimsaFromEither $
      Actions.run
        project
        ( Actions.typecheckExpression project input expr
        )
  replOutput $
    prettyPrint expr
      <> "/n:: "
      <> prettyPrint (reMonoType resolvedExpr)

----------
