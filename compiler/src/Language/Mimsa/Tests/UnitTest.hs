{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Tests.UnitTest
  ( createUnitTest,
  )
where

import Data.Bifunctor (first)
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Interpreter
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Tests.Helpers
import Language.Mimsa.Tests.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

-- | a unit test must have type Boolean
createUnitTest ::
  Project Annotation ->
  StoreExpression Annotation ->
  TestName ->
  Either (Error Annotation) UnitTest
createUnitTest project storeExpr testName = do
  let testExpr = exprEqualsTrue (storeExpression storeExpr)
  (ResolvedExpression _ _ rExpr rScope rSwaps _ _) <-
    Actions.getTypecheckedStoreExpression (prettyPrint testExpr) project testExpr
  result <- first InterpreterErr (interpret rScope rSwaps rExpr)
  pure $
    UnitTest
      { utName = testName,
        utSuccess = UnitTestSuccess (testIsSuccess result),
        utExprHash = getStoreExpressionHash storeExpr
      }
