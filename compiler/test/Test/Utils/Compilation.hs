{-# LANGUAGE OverloadedStrings #-}

module Test.Utils.Compilation
  ( testProjectCompile,
    testModuleCompile,
    testWholeProjectCompile,
  )
where

import Control.Monad.Except
import Data.Foldable
import Data.Hashable
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Mimsa.Actions.Compile as Actions
import qualified Language.Mimsa.Actions.Modules.Check as Actions
import qualified Language.Mimsa.Actions.Modules.Evaluate as Actions
import qualified Language.Mimsa.Actions.Modules.Imports as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Types as Actions
import Language.Mimsa.Backend.Output
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Core
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Utils.Helpers
import Test.Utils.Serialisation

-- | evaluate an expression, then compile into a temp folder and return the
-- main filename
testProjectCompile ::
  String ->
  Backend ->
  Expr Name Annotation ->
  IO (FilePath, Int)
testProjectCompile folderPrefix be expr = do
  let action = do
        (_, _, newModule) <- Actions.evaluateModule expr mempty
        (_, exprMap, _) <- Actions.compileModule be newModule
        let exprName = case Actions.evalId of
              DIName name -> name
              _ -> error "broken evalId"
        case M.lookup exprName exprMap of
          Just eh -> pure eh
          Nothing -> error "could not find outputted exprHash to compile"
  let (_newProject_, outcomes, seHash) =
        fromRight (Actions.run stdlib action)

  writeFiles be folderPrefix seHash outcomes

-- | compile a module into a temp folder and return the main filename
testModuleCompile ::
  String ->
  Backend ->
  T.Text ->
  IO (FilePath, Int)
testModuleCompile folderPrefix be input = do
  let action = do
        -- parse a module from text
        (parsedModule, _) <- Actions.checkModule mempty input
        -- turn into TS / JS etc
        (moduleHash, _, _) <- Actions.compileModule be (getAnnotationForType <$> parsedModule)
        pure moduleHash
  let (_newProject_, outcomes, modHash) =
        fromRight (Actions.run testStdlib action)

  writeModuleFiles be folderPrefix modHash outcomes

-- | compile a project into a temp folder and return the main filename
testWholeProjectCompile ::
  String ->
  Project Annotation ->
  Backend ->
  IO (FilePath, Int)
testWholeProjectCompile folderName project be = do
  let action = do
        _ <- Actions.compileProject be
        pure ()
  let (_newProject_, outcomes, _) =
        fromRight (Actions.run project action)

  -- clean up old rubbish
  deleteOutputFolder folderName

  -- re-create path
  tsPath <- createOutputFolder folderName

  -- write all files to temp folder
  traverse_
    ( \(_, filename, Actions.SaveContents content) -> do
        let savePath = tsPath <> show filename
        liftIO $ T.writeFile savePath content
    )
    (Actions.writeFilesFromOutcomes outcomes)

  -- hash of generated content for caching test results
  let allFilesHash = hash (Actions.writeFilesFromOutcomes outcomes)

  let actualIndexPath = tsPath <> "/" <> T.unpack (projectIndexFilename be)

  pure (actualIndexPath, allFilesHash)

writeModuleFiles :: Backend -> String -> ModuleHash -> [Actions.ActionOutcome] -> IO (FilePath, Int)
writeModuleFiles be folderPrefix modHash outcomes = do
  let folderName = folderPrefix <> "/compile-test-" <> show modHash

  -- clean up old rubbish
  deleteOutputFolder folderName

  -- re-create path
  tsPath <- createOutputFolder folderName

  -- write all files to temp folder
  traverse_
    ( \(_, filename, Actions.SaveContents content) -> do
        let savePath = tsPath <> show filename
        liftIO $ T.writeFile savePath content
    )
    (Actions.writeFilesFromOutcomes outcomes)

  -- hash of generated content for caching test results
  let allFilesHash = hash (Actions.writeFilesFromOutcomes outcomes)

  -- make a new index file that imports the outcome and logs it
  let actualIndex =
        "import { main } from './"
          <> moduleImport be modHash
          <> "';\nconsole.log(main)"

  -- get filename of index file
  let actualIndexPath = tsPath <> T.unpack (projectIndexFilename be)

  -- write actual index
  liftIO (T.writeFile actualIndexPath actualIndex)

  pure (actualIndexPath, allFilesHash)

writeFiles :: Backend -> String -> ExprHash -> [Actions.ActionOutcome] -> IO (FilePath, Int)
writeFiles be folderPrefix seHash outcomes = do
  let folderName = folderPrefix <> "/compile-test-" <> show seHash

  -- clean up old rubbish
  deleteOutputFolder folderName

  -- re-create path
  tsPath <- createOutputFolder folderName

  -- write all files to temp folder
  traverse_
    ( \(_, filename, Actions.SaveContents content) -> do
        let savePath = tsPath <> show filename
        liftIO $ T.writeFile savePath content
    )
    (Actions.writeFilesFromOutcomes outcomes)

  -- hash of generated content for caching test results
  let allFilesHash = hash (Actions.writeFilesFromOutcomes outcomes)

  -- make a new index file that imports the outcome and logs it
  let actualIndex =
        "import { main } from './"
          <> storeExprFilename be seHash
          <> "';\nconsole.log(main)"

  -- get filename of index file
  let actualIndexPath = tsPath <> T.unpack (projectIndexFilename be)

  -- write actual index
  liftIO (T.writeFile actualIndexPath actualIndex)

  pure (actualIndexPath, allFilesHash)
