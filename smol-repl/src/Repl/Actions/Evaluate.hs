{-# LANGUAGE OverloadedStrings #-}

module Repl.Actions.Evaluate
  ( doEvaluate,
  )
where

import qualified Language.Mimsa.Actions.Modules.Evaluate as Actions
import Language.Mimsa.Core
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Prettyprinter
import Repl.Helpers
import Repl.ReplM

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
