module Repl.Actions.UnitTests
  ( doAddTest,
    doListTests,
  )
where

import Data.Foldable
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Actions.AddUnitTest
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Tests.Test
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Tests
import Repl.Helpers
import Repl.ReplM

doAddTest ::
  Project Annotation ->
  Text ->
  TestName ->
  Expr Name Annotation ->
  ReplM (Error Annotation) (Project Annotation)
doAddTest project input testName expr = do
  (newProject, test) <-
    toReplM project (addUnitTest expr testName input)
  testResult <- runTests newProject test
  replOutput (prettyPrint testResult)
  pure newProject

doListTests ::
  Project Annotation -> Maybe Name -> ReplM (Error Annotation) ()
doListTests project maybeName = do
  let fetchTestsForName name =
        case lookupBindingName project name of
          Just exprHash -> getTestsForExprHash project exprHash
          Nothing -> mempty
  let tests = case maybeName of
        Just name -> fetchTestsForName name
        Nothing -> prjTests project
  testResult <- traverse (runTests project) (M.elems tests)
  traverse_ (replOutput . prettyPrint) testResult
