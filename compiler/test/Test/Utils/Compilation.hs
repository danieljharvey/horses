{-# LANGUAGE OverloadedStrings #-}

module Test.Utils.Compilation
  ( testProjectCompile,
  )
where

import Control.Monad.Except
import Data.Coerce
import Data.Foldable
import Data.Hashable
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Mimsa.Actions.Compile as Actions
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Backend.Types
import Language.Mimsa.Printer
import Language.Mimsa.Store.Storage
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Test.Data.Project
import Test.Utils.Helpers
import Test.Utils.Serialisation

-- | compile a project into a temp folder and return the main filename
testProjectCompile ::
  String ->
  Backend ->
  Expr Name Annotation ->
  IO (FilePath, Int)
testProjectCompile folderPrefix be expr = do
  let action = do
        (_, _, storeExpr, _, _) <- Actions.evaluate (prettyPrint expr) expr
        let seHash = getStoreExpressionHash storeExpr
        _ <- Actions.compile be storeExpr
        pure seHash
  let (_newProject_, outcomes, seHash) =
        fromRight (Actions.run testStdlib action)
  let folderName = folderPrefix <> "/compile-test-" <> show seHash

  -- clean up old rubbish
  deleteOutputFolder folderName

  -- re-create path
  tsPath <- createOutputFolder folderName

  -- write all files to temp folder
  traverse_
    ( \(_, filename, content) -> do
        let savePath = tsPath <> show filename
        -- hack to add the console.log, forgive me padre
        let content' =
              if "index" `isInfixOf` savePath
                then coerce content <> "console.log(main)"
                else coerce content
        liftIO $ T.writeFile savePath content'
    )
    (Actions.writeFilesFromOutcomes outcomes)

  -- hash of generated content for caching test results
  let allFilesHash = hash (Actions.writeFilesFromOutcomes outcomes)

  -- get filename of index file
  let indexPath = tsPath <> T.unpack (indexFilename be seHash)

  pure (indexPath, allFilesHash)
