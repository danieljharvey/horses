{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Backend
  ( outputCommonJS,
    goCompile,
    Backend (..),
  )
where

import Control.Monad.Except
import Data.Coerce
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Printer
import Language.Mimsa.Store.ResolvedDeps
import Language.Mimsa.Store.Storage (getStoreExpressionHash, getStoreFolder, trySymlink)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import System.Directory

data Backend
  = CommonJS

data Renderer ann a
  = Renderer
      { renderFunc :: Name -> Expr Name ann -> a,
        renderImport :: Backend -> (Name, ExprHash) -> a,
        renderStdLib :: Backend -> a,
        renderExport :: Backend -> Name -> a
      }

------

-- each expression is symlinked from the store to ./output/<exprhash>/<filename.ext>
createOutputFolder :: Backend -> ExprHash -> IO FilePath
createOutputFolder CommonJS exprHash = do
  let path = "./output/cjs" <> show exprHash
  createDirectoryIfMissing True path
  pure (path <> "/")

-- all files are created in the store and then symlinked into output folders
-- this creates the folder in the store
createStoreOutputPath :: Backend -> IO FilePath
createStoreOutputPath CommonJS = getStoreFolder "transpiled/common-js"

transpileStoreExpression ::
  (Monoid ann) =>
  Backend ->
  Store ann ->
  StoreExpression ann ->
  IO FilePath
transpileStoreExpression be store' se = do
  outputFolderPath <- createStoreOutputPath be
  let filename = outputFilename be (getStoreExpressionHash se)
  let path = T.pack outputFolderPath <> filename
  exists <-
    doesFileExist
      (T.unpack path)
  if exists
    then T.putStrLn $ path <> " already exists"
    else case resolveTypeDeps store' (storeTypeBindings se) of
      Left _ -> error "could not resolve types for output"
      Right dataTypes ->
        do
          let jsOutput = outputCommonJS dataTypes se
          T.putStrLn $ "Writing " <> path <> "..."
          T.writeFile (T.unpack path) (coerce jsOutput)
  pure
    (T.unpack path)

createIndexFile :: Backend -> ExprHash -> IO Text
createIndexFile CommonJS rootExprHash = do
  outputPath <- createOutputFolder CommonJS rootExprHash
  let file = "const main = require('./" <> outputFilename CommonJS rootExprHash <> "').main;\nconsole.log(main)"
      path = outputPath <> "index.js"
  T.writeFile path file
  pure (T.pack path)

-- we write the stdlib to the store and then symlink it
writeStdLib :: Backend -> ExprHash -> IO ()
writeStdLib CommonJS rootExprHash = do
  storePath <- createStoreOutputPath CommonJS
  outputPath <- createOutputFolder CommonJS rootExprHash
  let fromPath = T.pack storePath <> stdLibFilename CommonJS
  T.writeFile (T.unpack fromPath) (coerce commonJSStandardLibrary)
  let toPath = T.pack outputPath <> stdLibFilename CommonJS
  _ <- runExceptT $ trySymlink (T.unpack fromPath) (T.unpack toPath)
  pure ()

-- recursively get all the StoreExpressions we need to output
getOutputList :: (Ord ann) => Store ann -> StoreExpression ann -> Set (StoreExpression ann)
getOutputList store' se = case recursiveResolve store' se of
  Right as -> S.fromList as
  Left _ -> mempty

-- this recreates the folders which i dislike, however, yolo
symlinkOutput :: Set (StoreExpression ann) -> Backend -> ExprHash -> IO ()
symlinkOutput list CommonJS rootExprHash =
  do
    storePath <- createStoreOutputPath CommonJS
    outputPath <- createOutputFolder CommonJS rootExprHash
    let doLink se = do
          let filename = outputFilename CommonJS (getStoreExpressionHash se)
              fromPath = storePath <> T.unpack filename
              toPath = outputPath <> T.unpack filename
          runExceptT $ trySymlink fromPath toPath
    traverse_ doLink list

goCompile ::
  (Ord ann, Monoid ann) =>
  Backend ->
  Store ann ->
  StoreExpression ann ->
  IO Text
goCompile be store' se = do
  let list = getOutputList store' se
  let rootExprHash = getStoreExpressionHash se
  -- create output files in store if they don't exist
  traverse_ (transpileStoreExpression be store') list
  _ <- transpileStoreExpression be store' se
  -- create index file in output folder
  outputPath <- createIndexFile be rootExprHash
  -- write stdlib file in output folder
  writeStdLib be rootExprHash
  -- symlink all the files
  symlinkOutput (list <> S.singleton se) be rootExprHash
  -- return path of main filename
  pure outputPath

----

stdLibFilename :: Backend -> Text
stdLibFilename CommonJS = "cjs-stdlib.js"

outputFilename :: Backend -> ExprHash -> Text
outputFilename CommonJS hash' = "cjs-" <> prettyPrint hash' <> ".js"

outputExport :: Backend -> Name -> Text
outputExport CommonJS name = "module.exports = { " <> coerce name <> ": " <> coerce name <> " }"

outputStoreExpression :: (Monoid a) => Backend -> Renderer ann a -> StoreExpression ann -> a
outputStoreExpression be renderer se =
  let funcName = mkName "main"
      deps = mconcat $ renderImport renderer be <$> M.toList (getBindings $ storeBindings se)
      stdLib = renderStdLib renderer be
      func = renderFunc renderer funcName (storeExpression se)
      export = renderExport renderer be funcName
   in deps <> stdLib <> func <> export

outputCommonJS :: (Monoid ann) => ResolvedTypeDeps -> StoreExpression ann -> Javascript
outputCommonJS dataTypes =
  outputStoreExpression
    CommonJS
    Renderer
      { renderFunc = \name expr ->
          "const " <> coerce name <> " = "
            <> output dataTypes expr
            <> ";\n",
        renderImport = \be (name, hash') ->
          Javascript $
            "const "
              <> coerce name
              <> " = require(\"./"
              <> outputFilename be hash'
              <> "\").main;\n",
        renderExport = \be name -> Javascript $ outputExport be name,
        renderStdLib = \be ->
          let filename = stdLibFilename be
           in Javascript $ "const { __match, __eq } = require(\"./" <> filename <> "\");\n"
      }
