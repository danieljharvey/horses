module ReplNew.Actions.Evaluate
  ( doEvaluate,
  )
where

import Data.Text (Text)
import qualified Language.Mimsa.Actions.Evaluate as Actions
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import ReplNew.Helpers
import ReplNew.ReplM

doEvaluate ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  ReplM (Error Annotation) ()
doEvaluate project input expr = do
  _ <- toReplM project (Actions.evaluateModule input expr mempty)
  pure ()

---------
