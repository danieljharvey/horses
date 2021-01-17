{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Project.UnitTest (createUnitTest, getTestsForExprHash) where

import Data.Bifunctor (first)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Actions
import Language.Mimsa.Interpreter
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
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
  let testExpr = createUnitTestExpr (storeExpression storeExpr)
  (ResolvedExpression _ _ rExpr rScope rSwaps) <-
    getTypecheckedStoreExpression (prettyPrint testExpr) project testExpr
  result <- first InterpreterErr (interpret rScope rSwaps rExpr)
  let deps =
        S.fromList $
          M.elems (getBindings $ storeBindings storeExpr)
            <> M.elems (getTypeBindings $ storeTypeBindings storeExpr)
  pure $
    UnitTest
      { utName = testName,
        utSuccess = testIsSuccess result,
        utExprHash = getStoreExpressionHash storeExpr,
        utDeps = deps
      }

testIsSuccess :: Expr var ann -> TestSuccess
testIsSuccess (MyLiteral _ (MyBool True)) = TestSuccess True
testIsSuccess _ = TestSuccess False

createUnitTestExpr ::
  (Monoid ann) =>
  Expr Name ann ->
  Expr Name ann
createUnitTestExpr =
  MyInfix
    mempty
    Equals
    (MyLiteral mempty (MyBool True))

getTestsForExprHash :: Project ann -> ExprHash -> [UnitTest]
getTestsForExprHash _ _ = mempty
