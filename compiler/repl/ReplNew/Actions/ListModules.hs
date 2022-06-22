module ReplNew.Actions.ListModules
  ( doListModules,
  )
where

import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Project
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Project
import ReplNew.ReplM

showModule :: ModuleName -> ReplM e ()
showModule = replOutput

doListModules :: Project Annotation -> Text -> ReplM (Error Annotation) ()
doListModules project _input = do
  let moduleNames = M.keys (getCurrentModules $ prjModules project)
  traverse_ showModule moduleNames
