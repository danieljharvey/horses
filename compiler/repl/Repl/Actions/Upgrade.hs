module Repl.Actions.Upgrade
  ( doUpgrade,
  )
where

import qualified Language.Mimsa.Actions.Upgrade as Actions
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Repl.Helpers
import Repl.ReplM

doUpgrade ::
  Project Annotation ->
  Name ->
  ReplM (Error Annotation) ()
doUpgrade project name = do
  _ <- toReplM project (Actions.upgradeByName name)
  pure ()

---------
