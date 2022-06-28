{-# LANGUAGE OverloadedStrings #-}

module ReplNew.Actions.Bindings
  ( doAddBinding,
    doListBindings,
  )
where

import Data.Text (Text)
import qualified Language.Mimsa.Actions.BindModule as Actions
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Project
import ReplNew.Helpers
import ReplNew.ReplM

-- | add a binding to the global repl module
doAddBinding ::
  Project Annotation ->
  ModuleItem Annotation ->
  Text ->
  ReplM (Error Annotation) ()
doAddBinding project modItem input = do
  oldModule <- getStoredModule
  -- add the new binding
  (_prj, newModule) <- toReplM project (Actions.addBindingToModule oldModule modItem input)
  -- store the new module in Repl state
  setStoredModule newModule

-- | what is in the current implicit repl module
doListBindings :: ReplM (Error Annotation) ()
doListBindings = do
  oldModule <- getStoredModule
  -- output to console
  if oldModule == mempty
    then replOutput ("Current module is empty" :: Text)
    else replOutput oldModule
