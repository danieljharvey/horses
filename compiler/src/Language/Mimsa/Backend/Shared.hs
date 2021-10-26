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
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Coerce
import Data.FileEmbed
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Language.Mimsa.Backend.Types
import Language.Mimsa.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Store.ResolvedDeps
import Language.Mimsa.Store.Storage (getStoreFolder)
import Language.Mimsa.Typechecker.DataTypes
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

bsFromText :: Text -> LBS.ByteString
bsFromText = LB.fromChunks . return . T.encodeUtf8

-- these are saved in a file that is included in compilation
commonJSStandardLibrary :: LBS.ByteString
commonJSStandardLibrary =
  LBS.fromStrict $(embedFile "static/backend/commonjs/stdlib.js")

-- these are saved in a file that is included in compilation
esModulesJSStandardLibrary :: LBS.ByteString
esModulesJSStandardLibrary =
  LBS.fromStrict $(embedFile "static/backend/es-modules-js/stdlib.mjs")

stdLibFilename :: Backend -> LBS.ByteString
stdLibFilename CommonJS = "cjs-stdlib.js"
stdLibFilename ESModulesJS = "ejs-stdlib.mjs"
stdLibFilename Typescript = "ts-stdlib.ts"

indexOutputFilename :: Backend -> ExprHash -> LBS.ByteString
indexOutputFilename CommonJS exprHash =
  "index-" <> bsFromText (prettyPrint exprHash) <> ".js"
indexOutputFilename ESModulesJS exprHash =
  "index-" <> bsFromText (prettyPrint exprHash) <> ".mjs"
indexOutputFilename Typescript exprHash =
  "index-" <> bsFromText (prettyPrint exprHash) <> ".ts"

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

fileExtension :: Backend -> LBS.ByteString
fileExtension Typescript = ".ts"
fileExtension _ = ""

moduleFilename :: Backend -> ExprHash -> LBS.ByteString
moduleFilename CommonJS hash' =
  "cjs-" <> bsFromText (prettyPrint hash') <> ".js"
moduleFilename ESModulesJS hash' =
  "ejs-" <> bsFromText (prettyPrint hash') <> ".mjs"
moduleFilename Typescript hash' =
  "ts-" <> bsFromText (prettyPrint hash')

outputStdlib :: Backend -> LBS.ByteString
outputStdlib CommonJS =
  coerce commonJSStandardLibrary
outputStdlib ESModulesJS =
  coerce esModulesJSStandardLibrary
outputStdlib Typescript = mempty

outputExport :: Backend -> Name -> LBS.ByteString
outputExport CommonJS name =
  "module.exports = { " <> bsFromText (coerce name)
    <> ": "
    <> bsFromText (coerce name)
    <> " }"
outputExport ESModulesJS _ = mempty -- we export each one value directly
outputExport Typescript _ = mempty -- we export each one value directly

-- returns [Maybe, hash], [These, hash], [Either, hash] - used for imports
typeBindingsByType :: Store a -> TypeBindings -> Map TyCon ExprHash
typeBindingsByType store (TypeBindings tb) =
  let getTypeName exprHash =
        case lookupExprHashFromStore store exprHash of
          Just se -> storeExprToDataTypes se $> exprHash
          Nothing -> mempty
   in mconcat (getTypeName <$> M.elems tb)

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
