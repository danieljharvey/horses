{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.Evaluate
  ( doEvaluate,
  )
where

import Data.Text (Text)
import qualified Language.Mimsa.Actions.Evaluate as Actions
import Language.Mimsa.Monad
import Language.Mimsa.Repl.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project

doEvaluate ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  MimsaM (Error Annotation) ()
doEvaluate project input expr = do
  _ <- toReplM project (Actions.evaluate input expr)
  pure ()

---------
