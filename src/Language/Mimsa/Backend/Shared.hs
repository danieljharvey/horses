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

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LB
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

bsFromText :: Text -> LBS.ByteString
bsFromText = LB.fromChunks . return . T.encodeUtf8

-- these are saved in a file that is included in compilation
commonJSStandardLibrary :: LBS.ByteString
commonJSStandardLibrary =
  LBS.fromStrict $(embedFile "static/backend/commonjs/stdlib.js")

stdLibFilename :: Backend -> LBS.ByteString
stdLibFilename CommonJS = "cjs-stdlib.js"

indexFilename :: Backend -> LBS.ByteString
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

outputIndexFile :: Backend -> ExprHash -> LBS.ByteString
outputIndexFile CommonJS exprHash =
  "const main = require('./" <> moduleFilename CommonJS exprHash <> "').main;\nconsole.log(main)"

moduleFilename :: Backend -> ExprHash -> LBS.ByteString
moduleFilename CommonJS hash' = "cjs-" <> bsFromText (prettyPrint hash') <> ".js"

outputStdlib :: Backend -> LBS.ByteString
outputStdlib CommonJS = coerce commonJSStandardLibrary

outputExport :: Backend -> Name -> LBS.ByteString
outputExport CommonJS name =
  "module.exports = { " <> bsFromText (coerce name)
    <> ": "
    <> bsFromText (coerce name)
    <> " }"

outputStoreExpression ::
  (Monoid a) =>
  Backend ->
  Renderer ann a ->
  StoreExpression ann ->
  BackendM ann a
outputStoreExpression be renderer se = do
  let funcName = "main"
  deps <-
    traverse
      (renderImport renderer be)
      (M.toList (getBindings $ storeBindings se))
  stdLib <- renderStdLib renderer be
  func <- renderFunc renderer funcName (storeExpression se)
  export <- renderExport renderer be funcName
  pure $ mconcat deps <> stdLib <> func <> export

-- recursively get all the StoreExpressions we need to output
getTranspileList :: (Ord ann) => Store ann -> StoreExpression ann -> Set (StoreExpression ann)
getTranspileList store' se = case recursiveResolve store' se of
  Right as -> S.fromList as
  Left _ -> mempty
