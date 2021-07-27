module Language.Mimsa.Actions.RemoveBinding (removeBinding) where

import Control.Monad.Except
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Project as Prj
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers

-- remove a binding
removeBinding ::
  Name ->
  Actions.ActionM ()
removeBinding name = do
  project <- Actions.getProject
  case Prj.lookupBindingName project name of
    Just _ -> Actions.setProject (Prj.removeBinding project name)
    Nothing -> throwError (StoreErr $ CouldNotFindBinding name)
