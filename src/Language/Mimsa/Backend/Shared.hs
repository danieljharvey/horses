{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Backend.Shared
  ( transpiledModuleOutputPath,
    transpiledIndexOutputPath,
    transpiledStdlibOutputPath,
    symlinkedOutputPath,
    outputStoreExpression,
    outputExport,
    outputIndexFile,
    outputStdlib,
    indexFilename,
    moduleFilename,
    stdLibFilename,
    getTranspileList,
    commonJSStandardLibrary,
  )
where

import Data.Coerce
import Data.FileEmbed
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Language.Mimsa.Backend.Types
import Language.Mimsa.Printer
import Language.Mimsa.Store.ResolvedDeps
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

-- these are saved in a file that is included in compilation
commonJSStandardLibrary :: Text
commonJSStandardLibrary =
  T.decodeUtf8 $(embedFile "static/backend/commonjs/stdlib.js")

stdLibFilename :: Backend -> Text
stdLibFilename CommonJS = "cjs-stdlib.js"

indexFilename :: Backend -> Text
indexFilename CommonJS = "index.js"

symlinkedOutputPath :: Backend -> FilePath
symlinkedOutputPath CommonJS =
  "./output/cjs"

transpiledModuleOutputPath :: Backend -> FilePath
transpiledModuleOutputPath CommonJS = "transpiled/module/common-js"

transpiledIndexOutputPath :: Backend -> FilePath
transpiledIndexOutputPath CommonJS = "transpiled/index/common-js"

transpiledStdlibOutputPath :: Backend -> FilePath
transpiledStdlibOutputPath CommonJS = "transpiled/stdlib/common-js"

outputIndexFile :: Backend -> ExprHash -> Text
outputIndexFile CommonJS exprHash =
  "const main = require('./" <> moduleFilename CommonJS exprHash <> "').main;\nconsole.log(main)"

moduleFilename :: Backend -> ExprHash -> Text
moduleFilename CommonJS hash' = "cjs-" <> prettyPrint hash' <> ".js"

outputStdlib :: Backend -> Text
outputStdlib CommonJS = coerce commonJSStandardLibrary

outputExport :: Backend -> Name -> Text
outputExport CommonJS name = "module.exports = { " <> coerce name <> ": " <> coerce name <> " }"

outputStoreExpression :: (Monoid a) => Backend -> Renderer ann a -> StoreExpression ann -> a
outputStoreExpression be renderer se =
  let funcName = "main"
      deps = mconcat $ renderImport renderer be <$> M.toList (getBindings $ storeBindings se)
      stdLib = renderStdLib renderer be
      func = renderFunc renderer funcName (storeExpression se)
      export = renderExport renderer be funcName
   in deps <> stdLib <> func <> export

-- recursively get all the StoreExpressions we need to output
getTranspileList :: (Ord ann) => Store ann -> StoreExpression ann -> Set (StoreExpression ann)
getTranspileList store' se = case recursiveResolve store' se of
  Right as -> S.fromList as
  Left _ -> mempty
