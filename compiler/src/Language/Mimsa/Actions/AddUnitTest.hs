module Language.Mimsa.Actions.AddUnitTest where

import Control.Monad.Except (liftEither)
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.UnitTests.Types
import Language.Mimsa.UnitTests.UnitTest

-- add a new unit test

addUnitTest ::
  Expr Name Annotation ->
  TestName ->
  Text ->
  Actions.ActionM UnitTest
addUnitTest expr testName input = do
  project <- Actions.getProject
  resolvedExpr <-
    liftEither $ Actions.getTypecheckedStoreExpression input project expr
  let storeExpr = reStoreExpression resolvedExpr
  Actions.appendStoreExpression storeExpr
  test <- liftEither $ createUnitTest project storeExpr testName
  Actions.appendProject (fromUnitTest test storeExpr)
  pure test
