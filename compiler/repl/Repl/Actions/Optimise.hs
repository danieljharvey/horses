module Repl.Actions.Optimise
  ( doOptimise,
  )
where

import qualified Language.Mimsa.Actions.Optimise as Actions
import Language.Mimsa.Monad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Repl.Helpers

doOptimise ::
  Project Annotation ->
  Name ->
  MimsaM (Error Annotation) ()
doOptimise project name = do
  _ <- toReplM project (Actions.optimiseByName name)
  pure ()

---------
