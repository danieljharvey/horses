{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.UnitTests
  ( doAddUnitTest,
    doListTests,
  )
where

import Data.Foldable
import Data.Text (Text)
import Language.Mimsa.Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.UnitTest
import Language.Mimsa.Repl.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression

doAddUnitTest ::
  Project Annotation ->
  Text ->
  TestName ->
  Expr Name Annotation ->
  ReplM Annotation (Project Annotation)
doAddUnitTest project input testName expr = do
  (ResolvedExpression _ storeExpr _ _ _) <-
    liftRepl $ getTypecheckedStoreExpression input project expr
  test <- liftRepl $ createUnitTest project storeExpr testName
  replPrint (prettyTest test)
  pure $ fromUnitTest test <> project

prettyTest :: UnitTest -> Text
prettyTest test =
  let tickOrCross = case utSuccess test of
        (TestSuccess True) -> "+++ PASS +++"
        _ -> "--- FAIL ---"
   in tickOrCross <> " " <> prettyPrint (utName test)

doListTests ::
  Project Annotation -> Maybe Name -> ReplM Annotation ()
doListTests project maybeName = do
  let fetchTestsForName =
        \name -> case lookupBindingName project name of
          Just exprHash -> getTestsForExprHash project exprHash
          Nothing -> mempty
  let tests = case maybeName of
        Just name -> fetchTestsForName name
        Nothing -> prjUnitTests project
  traverse_ (replPrint . prettyTest) tests
