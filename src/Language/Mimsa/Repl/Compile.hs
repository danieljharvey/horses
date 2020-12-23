{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Compile
  ( doOutputJS,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Language.Mimsa.Actions
import Language.Mimsa.Backend.Backend (Backend (..), goCompile)
import Language.Mimsa.Repl.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression

doOutputJS ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  ReplM Annotation ()
doOutputJS env input expr = do
  (ResolvedExpression _ storeExpr' _ _ _) <-
    liftRepl $ getTypecheckedStoreExpression input env expr
  outputPath <- liftIO $ goCompile CommonJS (store env) storeExpr'
  replPrint ("Output to " <> outputPath)
