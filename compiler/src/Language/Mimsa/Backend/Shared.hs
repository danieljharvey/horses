{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Backend.Shared
  ( transpiledModuleOutputPath,
    transpiledIndexOutputPath,
    transpiledStdlibOutputPath,
    symlinkedOutputPath,
    zipFileOutputPath,
    outputExport,
    outputStdlib,
    indexOutputFilename,
    moduleFilename,
    stdLibFilename,
    getTranspileList,
    fileExtension,
    commonJSStandardLibrary,
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
import Language.Mimsa.Types.Identifiers
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
commonJSStandardLibrary :: Text
commonJSStandardLibrary =
  T.decodeUtf8 $(embedFile "static/backend/commonjs/stdlib.js")

-- these are saved in a file that is included in compilation
esModulesJSStandardLibrary :: Text
esModulesJSStandardLibrary =
  T.decodeUtf8 $(embedFile "static/backend/es-modules-js/stdlib.mjs")

stdLibFilename :: Backend -> Text
stdLibFilename CommonJS = "cjs-stdlib.js"
stdLibFilename ESModulesJS = "ejs-stdlib.mjs"
stdLibFilename Typescript = "ts-stdlib.ts"

indexOutputFilename :: Backend -> ExprHash -> Text
indexOutputFilename CommonJS exprHash =
  "index-" <> prettyPrint exprHash <> ".js"
indexOutputFilename ESModulesJS exprHash =
  "index-" <> prettyPrint exprHash <> ".mjs"
indexOutputFilename Typescript exprHash =
  "index-" <> prettyPrint exprHash <> ".ts"

symlinkedOutputPath :: Backend -> FilePath
symlinkedOutputPath CommonJS =
  "./output/cjs"
symlinkedOutputPath ESModulesJS =
  "./output/ejs"
symlinkedOutputPath Typescript =
  "./output/ts"

transpiledModuleOutputPath :: Backend -> FilePath
transpiledModuleOutputPath CommonJS = "transpiled/module/common-js"
transpiledModuleOutputPath ESModulesJS = "transpiled/module/es-modules-js"
transpiledModuleOutputPath Typescript = "transpiled/module/typescript"

transpiledIndexOutputPath :: Backend -> FilePath
transpiledIndexOutputPath CommonJS = "transpiled/index/common-js"
transpiledIndexOutputPath ESModulesJS = "transpiled/index/es-modules-js"
transpiledIndexOutputPath Typescript = "transpiled/index/typescript"

transpiledStdlibOutputPath :: Backend -> FilePath
transpiledStdlibOutputPath CommonJS = "transpiled/stdlib/common-js"
transpiledStdlibOutputPath ESModulesJS = "transpiled/stdlib/es-modules-js"
transpiledStdlibOutputPath Typescript = "transpiled/stdlib/typescript"

zipFileOutputPath :: Backend -> FilePath
zipFileOutputPath _ = "./output/zip"

fileExtension :: Backend -> Text
fileExtension Typescript = ".ts"
fileExtension _ = ""

moduleFilename :: Backend -> ExprHash -> Text
moduleFilename CommonJS hash' =
  "cjs-" <> (prettyPrint hash') <> ".js"
moduleFilename ESModulesJS hash' =
  "ejs-" <> (prettyPrint hash') <> ".mjs"
moduleFilename Typescript hash' =
  "ts-" <> (prettyPrint hash')

outputStdlib :: Backend -> Text
outputStdlib CommonJS =
  coerce commonJSStandardLibrary
outputStdlib ESModulesJS =
  coerce esModulesJSStandardLibrary
outputStdlib Typescript = mempty

outputExport :: Backend -> Name -> Text
outputExport CommonJS name =
  "module.exports = { " <> (coerce name)
    <> ": "
    <> (coerce name)
    <> " }"
outputExport ESModulesJS _ = mempty -- we export each one value directly
outputExport Typescript _ = mempty -- we export each one value directly

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
