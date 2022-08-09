module Language.Mimsa.Actions.Helpers.FindExistingBinding (findExistingBinding) where

import qualified Data.Map.Strict as M
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

findExistingBinding ::
  Name ->
  Project Annotation ->
  Maybe (StoreExpression Annotation)
findExistingBinding name prj =
  lookupBindingName prj name
    >>= \exprHash -> M.lookup exprHash (getStore $ prjStore prj)
