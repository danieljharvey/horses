{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Project.UnitTest (createUnitTest, getTestsForExprHash) where

import Data.Bifunctor (first)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Actions
import Language.Mimsa.Interpreter
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Swaps

-- | a unit test must have type Boolean
createUnitTest ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  TestName ->
  Either (Error Annotation) (UnitTest Annotation)
createUnitTest project input expr' testName = do
  let testExpr = createUnitTestExpr expr'
  (ResolvedExpression _ _ rExpr rScope rSwaps) <-
    getTypecheckedStoreExpression input project testExpr
  result <- first InterpreterErr (interpret rScope rSwaps rExpr)
  deps <- first OtherError (swapsToExprHash project (filterSwaps rScope rSwaps))
  pure $
    UnitTest
      { utName = testName,
        utSuccess = testIsSuccess result,
        utExpr = expr',
        utDeps = deps
      }

filterSwaps :: Scope a -> Swaps -> Swaps
filterSwaps (Scope scope) = M.filterWithKey (\k _ -> M.member k scope)

-- | get the hashes of functions that have been used
swapsToExprHash :: Project Annotation -> Swaps -> Either Text (Set ExprHash)
swapsToExprHash project swaps' =
  let names = M.elems swaps'
      lookupName = \name -> case lookupBindingName project name of
        Just expr -> Right expr
        _ -> Left ("Could not find expr for " <> prettyPrint name)
   in S.fromList <$> traverse lookupName names

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

getTestsForExprHash :: Project ann -> ExprHash -> [UnitTest ann]
getTestsForExprHash _ _ = mempty
