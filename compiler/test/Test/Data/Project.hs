
module Test.Data.Project
  ( testStdlib,
    addExprBinding,
  )
where

import Data.Functor
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression

-- check removing annotation doesn't break stuff
testStdlib :: Project Annotation
testStdlib = case buildTestStdlib of
  Right stdLib' -> (stdLib' $> ()) $> mempty
  Left e ->
    error (T.unpack $ prettyPrint e)

buildTestStdlib :: Either (Error Annotation) (Project Annotation)
buildTestStdlib =
  Actions.run mempty action >>= \(proj, _, _) -> pure proj
  where
    action = pure ()

addExprBinding ::
  Expr Name Annotation ->
  Name ->
  Project Annotation ->
  Either (Error Annotation) (Project Annotation)
addExprBinding expr name project = do
  (_, _, resolvedExpr) <-
    Actions.run
      project
      ( Actions.typecheckExpression project (prettyPrint expr) expr
      )
  let seUnit = reStoreExpression resolvedExpr $> ()
  let hash = getStoreExpressionHash seUnit
  let newEnv = fromItem name (reStoreExpression resolvedExpr) hash
  pure (project <> newEnv)
