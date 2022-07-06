{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Tests.UnitTest
  ( createUnitTest,
  )
where

import qualified Language.Mimsa.Actions.Interpret as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Tests.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Tests

-- | a unit test must have type Boolean
createUnitTest ::
  Project Annotation ->
  StoreExpression Annotation ->
  TestName ->
  Either (Error Annotation) UnitTest
createUnitTest project storeExpr testName = do
  let testExpr = exprEqualsTrue (storeExpression storeExpr)
  (_, _, result) <-
    Actions.run
      project
      ( do
          resolved <- Actions.typecheckExpression project (prettyPrint testExpr) testExpr
          Actions.interpreter (reStoreExpression resolved)
      )
  pure $
    UnitTest
      { utName = testName,
        utSuccess = UnitTestSuccess (testIsSuccess result),
        utExprHash = getStoreExpressionHash storeExpr
      }
