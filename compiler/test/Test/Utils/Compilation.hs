{-# LANGUAGE OverloadedStrings #-}

module Test.Utils.Compilation
  ( testProjectCompile,
  )
where

import Control.Monad.Except
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.Foldable
import qualified Language.Mimsa.Actions.Compile as Actions
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Printer
import Language.Mimsa.Store.Storage
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Test.Backend.RunNode hiding (spec)
import Test.Data.Project
import Test.Utils.Helpers
import Test.Utils.Serialisation

-- | compile a project into a temp folder and return the main filename
testProjectCompile ::
  Runtime Javascript ->
  Expr Name Annotation ->
  IO FilePath
testProjectCompile rt expr = do
  let action = do
        (_, _, storeExpr, _, _, _) <- Actions.evaluate (prettyPrint expr) expr
        let seHash = getStoreExpressionHash storeExpr
        _ <- Actions.compile rt "id" storeExpr
        pure seHash
  let (_newProject_, outcomes, seHash) =
        fromRight (Actions.run testStdlib action)
  let folderName = "CompileProject/compile-test-" <> show seHash

  -- clean up old rubbish
  deleteOutputFolder folderName

  -- re-create path
  tsPath <- createOutputFolder folderName

  -- write all files to temp folder
  traverse_
    ( \(_, filename, content) -> do
        let savePath = tsPath <> show filename
        liftIO $ LBS.writeFile savePath (coerce content)
    )
    (Actions.writeFilesFromOutcomes outcomes)

  -- get filename of index file
  pure $ tsPath <> lbsToString (indexFilename rt seHash)
