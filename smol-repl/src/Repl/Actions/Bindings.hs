{-# LANGUAGE OverloadedStrings #-}

module Repl.Actions.Bindings
  ( doAddBinding,
    doListBindings,
  )
where

import Data.Text (Text)
import qualified Language.Mimsa.Actions.Modules.Bind as Actions
import Language.Mimsa.Core
import Language.Mimsa.Modules.Pretty
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Repl.Helpers
import Repl.ReplM

-- | add a binding to the global repl module
doAddBinding ::
  Project Annotation ->
  ModuleItem Annotation ->
  ReplM (Error Annotation) ()
doAddBinding project modItem = do
  oldModule <- getStoredModule
  -- add the new binding
  (_prj, (newModule, testResults)) <- toReplM project (Actions.addBindingToModule mempty oldModule modItem)

  -- show test results
  replOutput testResults

  -- store the new module in Repl state
  setStoredModule newModule

-- | what is in the current implicit repl module
doListBindings :: ReplM (Error Annotation) ()
doListBindings = do
  oldModule <- getStoredModule
  -- output to console
  if oldModule == mempty
    then replOutput ("Current module is empty" :: Text)
    else replDocOutput (modulePretty oldModule)
