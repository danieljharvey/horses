{-# LANGUAGE OverloadedStrings #-}

module ReplNew.Actions.Evaluate
  ( doEvaluate,
  )
where

import qualified Language.Mimsa.Actions.Modules.Evaluate as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Typechecker
import Prettyprinter
import ReplNew.Helpers
import ReplNew.ReplM

doEvaluate ::
  Project Annotation ->
  Expr Name Annotation ->
  ReplM (Error Annotation) ()
doEvaluate project expr = do
  oldModule <- fmap getAnnotationForType <$> getStoredModule

  (_prj, (exprType, evaluatedExpression, _)) <-
    toReplM project (Actions.evaluateModule expr oldModule)

  -- print
  replDocOutput
    ( group
        ( prettyDoc evaluatedExpression
            <> line
            <> "::"
            <> line
            <> prettyDoc exprType
        )
    )

  pure ()

---------
