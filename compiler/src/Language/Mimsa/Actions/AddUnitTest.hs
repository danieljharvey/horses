module Language.Mimsa.Actions.AddUnitTest where

import Control.Monad.Except (liftEither)
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Tests.Test
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Tests

-- add a new unit test

addUnitTest ::
  Expr Name Annotation ->
  TestName ->
  Text ->
  Actions.ActionM Test
addUnitTest expr testName input = do
  project <- Actions.getProject
  resolvedExpr <-
    Actions.typecheckExpression project input expr
  let storeExpr = reStoreExpression resolvedExpr
  Actions.appendStoreExpression storeExpr
  test <- liftEither $ createTest project storeExpr testName
  Actions.appendProject (fromTest test storeExpr)
  pure test
