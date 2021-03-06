{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.ExpressionBind
  ( doBind,
    doBindType,
  )
where

import Data.Foldable
import Data.Text (Text)
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.BindType as Actions
import Language.Mimsa.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Project.UnitTest
import Language.Mimsa.Repl.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project

doBind ::
  Project Annotation ->
  Text ->
  Name ->
  Expr Name Annotation ->
  MimsaM (Error Annotation) (Project Annotation)
doBind project input name expr = do
  (newProject, (newExprHash, _, _)) <-
    toReplM project (Actions.bindExpression expr name input)
  traverse_
    (replOutput . prettyPrint)
    (getTestsForExprHash newProject newExprHash)
  pure newProject

doBindType ::
  Project Annotation ->
  Text ->
  DataType ->
  MimsaM (Error Annotation) (Project Annotation)
doBindType project input dt = do
  (newProject, _) <-
    toReplM project (Actions.bindType input dt)
  pure newProject
