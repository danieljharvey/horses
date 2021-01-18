{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.AddUnitTest
  ( doAddUnitTest,
  )
where

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
  let tickOrCross = case utSuccess test of
        (TestSuccess True) -> "+++ PASS +++"
        _ -> "--- FAIL ---"
  replPrint $
    tickOrCross <> " '" <> prettyPrint testName <> "'\n\n"
      <> prettyPrint expr
  pure $ fromUnitTest test <> project
