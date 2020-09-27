{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Backend
  ( assembleCommonJS,
    outputCommonJS,
    goCompile,
    Backend (..),
  )
where

import Data.Bifunctor (first)
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
  ( recursiveResolve,
    recursiveResolveSE,
  )
import Language.Mimsa.Store.Storage (getStoreExpressionHash)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Bindings
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.StoreExpression
import Language.Mimsa.Types.Usage
import System.Directory

data Backend
  = CommonJS

data Renderer a
  = Renderer
      { renderFunc :: Name -> Expr Name -> a,
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

transpileStoreExpression :: Backend -> StoreExpression -> IO FilePath
transpileStoreExpression be se = do
  _ <- createOutputFolder
  let jsOutput = outputCommonJS se
  let filename = outputFilename be (getStoreExpressionHash se)
  let path = "./output/" <> filename
  T.putStrLn $ "Writing " <> path <> "..."
  T.writeFile (T.unpack path) (coerce jsOutput)
  pure (T.unpack path)

-- recursively get all the StoreExpressions we need to output
getOutputList :: Store -> StoreExpression -> Set StoreExpression
getOutputList store' se = case recursiveResolveSE store' se of
  Right as -> S.fromList as
  Left _ -> mempty

goCompile :: Backend -> Store -> StoreExpression -> IO ()
goCompile be store' se = do
  let list = getOutputList store' se
  traverse_ (transpileStoreExpression be) list

----

stdLibFilename :: Backend -> Text
stdLibFilename CommonJS = "cjs-stdlib.js"

outputFilename :: Backend -> ExprHash -> Text
outputFilename CommonJS hash' = "cjs-" <> prettyPrint hash' <> ".js"

outputExport :: Backend -> Name -> Text
outputExport CommonJS name = "module.exports = { " <> coerce name <> ": " <> coerce name <> " }"

outputStoreExpression :: (Monoid a) => Backend -> Renderer a -> StoreExpression -> a
outputStoreExpression be renderer se =
  let funcName = mkName "main"
      deps = mconcat $ renderImport renderer be <$> M.toList (getBindings $ storeBindings se)
      stdLib = renderStdLib renderer be
      func = renderFunc renderer funcName (storeExpression se)
      export = renderExport renderer be funcName
   in deps <> stdLib <> func <> export

outputCommonJS :: StoreExpression -> Javascript
outputCommonJS =
  outputStoreExpression
    CommonJS
    Renderer
      { renderFunc = \name expr ->
          "const " <> coerce name <> " = "
            <> output expr
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
           in Javascript $ "const { __app, __match } = require(\"." <> filename <> "\");\n"
      }

assemble ::
  (Monoid a) =>
  (Name -> Expr Name -> a) ->
  Store ->
  StoreExpression ->
  Name ->
  Either UsageError a
assemble render store storeExpr name = do
  deps <- first CouldNotResolveDeps $ recursiveResolve store storeExpr
  let renderWithName = uncurry render
  pure $ foldMap renderWithName deps <> render name (storeExpression storeExpr)

assembleCommonJS :: Store -> StoreExpression -> Name -> Either UsageError Javascript
assembleCommonJS store' storeExpr name' = do
  jsOutput <-
    assemble
      (\name expr -> "const " <> coerce name <> " = " <> output expr <> ";\n")
      store'
      storeExpr
      name'
  let exports = "module.exports = { " <> coerce name' <> ": " <> coerce name' <> "}"
  pure $ commonJSStandardLibrary <> jsOutput <> exports
