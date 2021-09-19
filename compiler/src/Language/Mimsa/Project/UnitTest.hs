{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Project.UnitTest (createUnitTest, getTestsForExprHash, createNewUnitTests) where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Interpreter
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store
import Language.Mimsa.Store.UpdateDeps
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
  (ResolvedExpression _ _ rExpr rScope rSwaps _ _) <-
    Actions.getTypecheckedStoreExpression (prettyPrint testExpr) project testExpr
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

getTestsForExprHash :: Project ann -> ExprHash -> Map ExprHash UnitTest
getTestsForExprHash prj exprHash =
  M.filter (S.member exprHash . utDeps) (prjUnitTests prj)

updateUnitTest ::
  Project Annotation ->
  ExprHash ->
  ExprHash ->
  UnitTest ->
  Either (Error Annotation) (StoreExpression Annotation, UnitTest)
updateUnitTest project oldHash newHash unitTest = do
  case lookupExprHash project (utExprHash unitTest) of
    Nothing ->
      throwError
        ( StoreErr $ CouldNotFindStoreExpression (utExprHash unitTest)
        )
    Just testStoreExpr -> do
      let newBindings =
            updateExprHash testStoreExpr oldHash newHash
      newTestStoreExpr <-
        updateStoreExpressionBindings project newBindings testStoreExpr
      (,) newTestStoreExpr <$> createUnitTest project newTestStoreExpr (utName unitTest)

createNewUnitTests ::
  Project Annotation ->
  ExprHash ->
  ExprHash ->
  Either (Error Annotation) (Project Annotation, [StoreExpression Annotation])
createNewUnitTests project oldHash newHash = do
  let tests = getTestsForExprHash project oldHash
  newTests <-
    traverse
      (updateUnitTest project oldHash newHash)
      (M.elems tests)
  let newProject =
        project
          <> mconcat
            ( (\(se, ut) -> fromUnitTest ut se)
                <$> newTests
            )
  let exprs = fst <$> newTests
  pure (newProject, exprs)
