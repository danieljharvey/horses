module Language.Mimsa.Actions.AddUnitTest where

import Control.Monad.Except (liftEither)
import Data.Text (Text)
import Language.Mimsa.Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.UnitTest
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression

-- add a new unit test

addUnitTest ::
  Expr Name Annotation ->
  TestName ->
  Text ->
  Actions.ActionM UnitTest
addUnitTest expr testName input = do
  project <- Actions.getProject
  (ResolvedExpression _ storeExpr _ _ _) <-
    liftEither $ getTypecheckedStoreExpression input project expr
  Actions.appendStoreExpression storeExpr
  test <- liftEither $ createUnitTest project storeExpr testName
  Actions.appendProject (fromUnitTest test storeExpr)
  pure test
