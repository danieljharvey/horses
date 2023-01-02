{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ReplNew.Actions.ListModules
  ( doListModules,
  )
where

import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Helpers.LookupExpression as Actions
import qualified Language.Mimsa.Actions.Modules.Typecheck as Actions
import Language.Mimsa.Modules.Pretty
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Project
import ReplNew.Helpers
import ReplNew.ReplM

-- | get module from project
-- | typecheck it
-- | pretty display it
showModule :: Project Annotation -> ModuleHash -> ReplM (Error Annotation) ()
showModule prj modHash = do
  let action = do
        mod' <- Actions.lookupModule modHash
        Actions.typecheckModule (prettyPrint mod') mod'
  (_, typedMod) <- toReplM prj action
  replDocOutput (modulePretty typedMod)

doListModules :: Project Annotation -> Maybe ModuleName -> ReplM (Error Annotation) ()
doListModules project Nothing = do
  let moduleNames = M.keys (getCurrentModules $ prjModules project)
  traverse_ replOutput moduleNames
doListModules project (Just modName) = do
  case M.lookup modName (getCurrentModules $ prjModules project) of
    Just moduleHash -> showModule project moduleHash
    Nothing -> replOutput @Text $ "Could not find module " <> prettyPrint modName
