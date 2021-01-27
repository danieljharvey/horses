{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.Compile
  ( doOutputJS,
  )
where

import Control.Monad.Reader
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Compile as Actions
import Language.Mimsa.Backend.Backend (Backend (..), goCompile)
import Language.Mimsa.Repl.Helpers
import Language.Mimsa.Repl.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project

doOutputJS ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  ReplM Annotation ()
doOutputJS project input expr = do
  (newProject, _) <-
    toReplM project (Actions.compile CommonJS input expr)
  mimsaConfig <- ask
  outputPath <-
    liftIO $
      goCompile mimsaConfig CommonJS (prjStore newProject) undefined
  replPrint ("Output to " <> outputPath)
