module ReplNew.Actions.Evaluate
  ( doEvaluate,
  )
where

import qualified Language.Mimsa.Actions.Evaluate as Actions
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Typechecker
import ReplNew.Helpers
import ReplNew.ReplM

doEvaluate ::
  Project Annotation ->
  Expr Name Annotation ->
  ReplM (Error Annotation) ()
doEvaluate project expr = do
  oldModule <- fmap getAnnotationForType <$> getStoredModule

  _ <- toReplM project (Actions.evaluateModule expr oldModule)
  pure ()

---------
