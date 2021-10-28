{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Backend.Shared
  ( transpiledModuleOutputPath,
    transpiledIndexOutputPath,
    transpiledStdlibOutputPath,
    symlinkedOutputPath,
    esModulesJSStandardLibrary,
    zipFileOutputPath,
    outputStdlib,
    indexOutputFilename,
    moduleFilename,
    stdLibFilename,
    getTranspileList,
    fileExtension,
    createOutputFolder,
    createModuleOutputPath,
    createStdlibOutputPath,
    createIndexOutputPath,
  )
where

import Control.Monad.IO.Class
import Data.Coerce
import Data.FileEmbed
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Language.Mimsa.Backend.Types
import Language.Mimsa.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Store.ResolvedDeps
import Language.Mimsa.Store.Storage (getStoreFolder)
import Language.Mimsa.Types.Store
import System.Directory

-- each expression is symlinked from the store to ./output/<exprhash>/<filename.ext>
createOutputFolder :: Backend -> ExprHash -> MimsaM e FilePath
createOutputFolder be exprHash = do
  let outputPath = symlinkedOutputPath be
  let path = outputPath <> show exprHash
  liftIO $ createDirectoryIfMissing True path
  pure (path <> "/")

-- all files are created in the store and then symlinked into output folders
-- this creates the folder in the store
createModuleOutputPath :: Backend -> MimsaM e FilePath
createModuleOutputPath be =
  getStoreFolder (transpiledModuleOutputPath be)

-- all files are created in the store and then symlinked into output folders
-- this creates the folder in the store
createIndexOutputPath :: Backend -> MimsaM e FilePath
createIndexOutputPath be =
  getStoreFolder (transpiledIndexOutputPath be)

-- all files are created in the store and then symlinked into output folders
-- this creates the folder in the store
createStdlibOutputPath :: Backend -> MimsaM e FilePath
createStdlibOutputPath be =
  getStoreFolder (transpiledStdlibOutputPath be)

-- these are saved in a file that is included in compilation
esModulesJSStandardLibrary :: Text
esModulesJSStandardLibrary =
  T.decodeUtf8 $(embedFile "static/backend/es-modules-js/stdlib.mjs")

stdLibFilename :: Backend -> Text
stdLibFilename ESModulesJS = "ejs-stdlib.mjs"
stdLibFilename Typescript = "ts-stdlib.ts"

indexOutputFilename :: Backend -> ExprHash -> Text
indexOutputFilename ESModulesJS exprHash =
  "index-" <> prettyPrint exprHash <> ".mjs"
indexOutputFilename Typescript exprHash =
  "index-" <> prettyPrint exprHash <> ".ts"

symlinkedOutputPath :: Backend -> FilePath
symlinkedOutputPath ESModulesJS =
  "./output/ejs"
symlinkedOutputPath Typescript =
  "./output/ts"

transpiledModuleOutputPath :: Backend -> FilePath
transpiledModuleOutputPath ESModulesJS = "transpiled/module/es-modules-js"
transpiledModuleOutputPath Typescript = "transpiled/module/typescript"

transpiledIndexOutputPath :: Backend -> FilePath
transpiledIndexOutputPath ESModulesJS = "transpiled/index/es-modules-js"
transpiledIndexOutputPath Typescript = "transpiled/index/typescript"

transpiledStdlibOutputPath :: Backend -> FilePath
transpiledStdlibOutputPath ESModulesJS = "transpiled/stdlib/es-modules-js"
transpiledStdlibOutputPath Typescript = "transpiled/stdlib/typescript"

zipFileOutputPath :: Backend -> FilePath
zipFileOutputPath _ = "./output/zip"

fileExtension :: Backend -> Text
fileExtension Typescript = ".ts"
fileExtension _ = ""

moduleFilename :: Backend -> ExprHash -> Text
moduleFilename ESModulesJS hash' =
  "ejs-" <> prettyPrint hash' <> ".mjs"
moduleFilename Typescript hash' =
  "ts-" <> prettyPrint hash'

outputStdlib :: Backend -> Text
outputStdlib ESModulesJS =
  coerce esModulesJSStandardLibrary
outputStdlib Typescript = mempty

-- recursively get all the StoreExpressions we need to output
getTranspileList ::
  (Ord ann) =>
  Store ann ->
  StoreExpression ann ->
  Set (StoreExpression ann)
getTranspileList store' se =
  case recursiveResolve store' se of
    Right as -> S.fromList as
    Left _ -> mempty
