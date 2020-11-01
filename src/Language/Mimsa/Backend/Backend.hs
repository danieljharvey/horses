{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Backend
  ( outputCommonJS,
    goCompile,
    Backend (..),
  )
where

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
import Language.Mimsa.Store.Storage (getStoreExpressionHash)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Bindings
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedTypeDeps
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.StoreExpression
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

createOutputFolder :: IO FilePath
createOutputFolder = do
  let path = "./output"
  createDirectoryIfMissing True path
  pure (path <> "/")

transpileStoreExpression :: (Monoid ann) => Backend -> Store ann -> StoreExpression ann -> IO FilePath
transpileStoreExpression be store' se = do
  _ <- createOutputFolder
  let filename = outputFilename be (getStoreExpressionHash se)
  let path = "./output/" <> filename
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

createIndexFile :: Backend -> ExprHash -> IO ()
createIndexFile CommonJS hash' = do
  let file = "const main = require('./" <> outputFilename CommonJS hash' <> "').main;\nconsole.log(main)"
      path = "./output/index.js"
  T.writeFile path file

writeStdLib :: Backend -> IO ()
writeStdLib CommonJS = do
  let path = "./output/" <> stdLibFilename CommonJS
  T.writeFile (T.unpack path) (coerce commonJSStandardLibrary)

-- recursively get all the StoreExpressions we need to output
getOutputList :: (Ord ann) => Store ann -> StoreExpression ann -> Set (StoreExpression ann)
getOutputList store' se = case recursiveResolve store' se of
  Right as -> S.fromList as
  Left _ -> mempty

goCompile :: (Ord ann, Monoid ann) => Backend -> Store ann -> StoreExpression ann -> IO ()
goCompile be store' se = do
  let list = getOutputList store' se
  traverse_ (transpileStoreExpression be store') list
  _ <- transpileStoreExpression be store' se
  createIndexFile be (getStoreExpressionHash se)
  writeStdLib be
  pure ()

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
           in Javascript $ "const { __app, __match, __eq } = require(\"./" <> filename <> "\");\n"
      }
