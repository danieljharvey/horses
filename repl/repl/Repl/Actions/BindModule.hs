{-# LANGUAGE OverloadedStrings #-}

module Repl.Actions.BindModule
  ( doBindModule,
  )
where

import qualified Language.Mimsa.Actions.Modules.Bind as Actions
import Language.Mimsa.Core
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Repl.Helpers
import Repl.ReplM

-- | give the global module a name and clear it
doBindModule ::
  Project Annotation ->
  ModuleName ->
  ReplM (Error Annotation) (Project Annotation)
doBindModule project modName = do
  typedModule <- getStoredModule

  let untypedModule = getAnnotationForType <$> typedModule

  -- add the new binding
  (newProject, _) <-
    toReplM
      project
      (Actions.bindModule untypedModule modName (prettyPrint typedModule))

  replOutput $ "Stored repl module to " <> prettyPrint modName

  replDocOutput (prettyDoc typedModule)

  -- clear the module in Repl state
  setStoredModule mempty

  pure newProject
