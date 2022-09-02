module Repl.Actions.ExpressionBind
  ( doBind,
    doBindType,
  )
where

import Data.Text (Text)
import qualified Language.Mimsa.Actions.BindExpression as Actions
import qualified Language.Mimsa.Actions.BindType as Actions
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Repl.Helpers
import Repl.ReplM

doBind ::
  Project Annotation ->
  Text ->
  Name ->
  Expr Name Annotation ->
  ReplM (Error Annotation) (Project Annotation)
doBind project input name expr = do
  (newProject, _ ) <-
    toReplM project (Actions.bindExpression expr name input)
  pure newProject

doBindType ::
  Project Annotation ->
  Text ->
  DataType ->
  ReplM (Error Annotation) (Project Annotation)
doBindType project input dt = do
  (newProject, _) <-
    toReplM project (Actions.bindType input dt)
  pure newProject
