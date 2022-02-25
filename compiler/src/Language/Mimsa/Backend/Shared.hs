{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Backend.Shared
  ( transpiledModuleOutputPath,
    transpiledIndexOutputPath,
    transpiledStdlibOutputPath,
    symlinkedOutputPath,
    zipFileOutputPath,
    indexOutputFilename,
    moduleFilename,
    fileExtension,
    createOutputFolder,
    createModuleOutputPath,
    createIndexOutputPath,
    createStdlibOutputPath,
    stdlibFilename,
    outputStdlib,
  )
where

import Control.Monad.IO.Class
import Data.FileEmbed
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Language.Mimsa.Backend.Types
import Language.Mimsa.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Store.Storage (getStoreFolder)
import Language.Mimsa.Types.Store
import System.Directory

-- these are saved in a file that is included in compilation
typescriptStandardLibrary :: Text
typescriptStandardLibrary =
  T.decodeUtf8 $(embedFile "static/backend/typescript/stdlib.ts")

-- these are saved in a file that is included in compilation
esModulesJSStandardLibrary :: Text
esModulesJSStandardLibrary =
  T.decodeUtf8 $(embedFile "static/backend/es-modules-js/stdlib.mjs")

outputStdlib :: Backend -> Text
outputStdlib Typescript = typescriptStandardLibrary
outputStdlib ESModulesJS = esModulesJSStandardLibrary

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

-- all files are created in the store and then symlinked into output folders
-- this creates the folder in the store
createStdlibOutputPath :: Backend -> MimsaM e FilePath
createStdlibOutputPath be =
  getStoreFolder (transpiledStdlibOutputPath be)

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

stdlibFilename :: Backend -> Text
stdlibFilename Typescript = "ts-stdlib"
stdlibFilename ESModulesJS = "ejs-stdlib.mjs"
