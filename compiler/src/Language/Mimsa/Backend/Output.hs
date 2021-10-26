{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Output (outputStoreExpression) where

import Control.Monad.Except
import Data.Coerce
import Data.Functor
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import qualified Language.Mimsa.Backend.Typescript.FromExpr as TS
import qualified Language.Mimsa.Backend.Typescript.Monad as TS
import qualified Language.Mimsa.Backend.Typescript.Printer as TS
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

-- returns [Maybe, hash], [These, hash], [Either, hash] - used for imports
typeBindingsByType :: Store a -> TypeBindings -> Map TyCon ExprHash
typeBindingsByType store (TypeBindings tb) =
  let getTypeName exprHash =
        case lookupExprHashFromStore store exprHash of
          Just se -> storeExprToDataTypes se $> exprHash
          Nothing -> mempty
   in mconcat (getTypeName <$> M.elems tb)

outputStoreExpression ::
  Backend ->
  ResolvedTypeDeps ->
  Store any ->
  MonoType ->
  StoreExpression MonoType ->
  BackendM MonoType Text
outputStoreExpression be dataTypes store mt se = do
  let funcName = "main"
  let deps =
        renderImport' be
          <$> M.toList (getBindings $ storeBindings se)
  let typeDeps =
        renderTypeImport' be
          <$> M.toList (typeBindingsByType store (storeTypeBindings se))
  let stdLib = renderStdlib' be
  func <- renderWithFunction Typescript dataTypes (storeExpression se)
  let typeComment = renderTypeSignature' be mt
  let export = renderExport' be funcName
  pure $
    mconcat
      ( intersperse
          (renderNewline' be)
          [ mconcat deps,
            mconcat typeDeps,
            stdLib,
            typeComment,
            func,
            export
          ]
      )

renderWithFunction :: Backend -> ResolvedTypeDeps -> Expr Name MonoType -> BackendM MonoType Text
renderWithFunction Typescript = renderTypescript
renderWithFunction _ = error "not implemented"

renderTypescript :: ResolvedTypeDeps -> Expr Name MonoType -> BackendM MonoType Text
renderTypescript dataTypes expr = do
  let readerState = TS.TSReaderState (makeTypeDepMap dataTypes)
   in case TS.fromExpr readerState expr of
        Right ts -> pure (TS.printModule ts)
        Left e -> throwError e

makeTypeDepMap :: ResolvedTypeDeps -> Map TyCon TyCon
makeTypeDepMap (ResolvedTypeDeps rtd) =
  (\(_, DataType typeName _ _) -> typeName) <$> rtd

renderImport' :: Backend -> (Name, ExprHash) -> Text
renderImport' Typescript (name, hash') =
  "import { main as "
    <> coerce name
    <> " } from \"./"
    <> moduleFilename Typescript hash'
    <> "\";\n"
renderImport' _ _ = error "no renderimport for js"

renderTypeImport' :: Backend -> (TyCon, ExprHash) -> Text
renderTypeImport' Typescript (typeName, hash') =
  "import * as "
    <> coerce typeName
    <> " from \"./"
    <> moduleFilename Typescript hash'
    <> "\";\n"
renderTypeImport' _ _ = error "no rendertypeimport for js"

renderStdlib' :: Backend -> Text
renderStdlib' Typescript = ""
renderStdlib' _ = error "no stdlib for js"

renderTypeSignature' :: Backend -> MonoType -> Text
renderTypeSignature' Typescript mt =
  "/* \n" <> prettyPrint mt <> "\n */"
renderTypeSignature' _ _ = error "no render type signature for js"

renderNewline' :: Backend -> Text
renderNewline' _ = "\n"

renderExport' :: Backend -> Name -> Text
renderExport' Typescript _ = ""
renderExport' _ _ = error "no render export for js"
