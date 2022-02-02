{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Output (outputStoreExpression) where

import Control.Monad.Except
import Data.Coerce
import Data.Functor
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Backend.Javascript.Printer as JS
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import qualified Language.Mimsa.Backend.Typescript.FromExpr as TS
import qualified Language.Mimsa.Backend.Typescript.Monad as TS
import qualified Language.Mimsa.Backend.Typescript.Printer as TS
import qualified Language.Mimsa.Backend.Typescript.Types as TS
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
  let deps =
        renderImport' be
          <$> M.toList (getBindings $ storeBindings se)
  let typeDeps =
        renderTypeImport' be
          <$> M.toList (typeBindingsByType store (storeTypeBindings se))
  (func, stdlibFuncs) <- renderExpression be dataTypes (storeExpression se)
  let stdlib = stdlibImport be stdlibFuncs
  let typeComment = renderTypeSignature' mt
  pure $
    mconcat
      ( intersperse
          (renderNewline' be)
          [ mconcat deps,
            mconcat typeDeps,
            stdlib,
            typeComment,
            func
          ]
      )

-- | given the fns used in a store expression
-- return an import
stdlibImport :: Backend -> [TS.TSImport] -> Text
stdlibImport _ [] = ""
stdlibImport backend names =
  let filteredNames = case backend of
        Typescript -> prettyPrint <$> names
        ESModulesJS ->
          prettyPrint
            <$> filter
              ( \case
                  TS.TSImportValue _ -> True
                  _ -> False
              )
              names
   in "import { " <> T.intercalate ", " filteredNames <> " } from \"./"
        <> stdlibFilename backend
        <> "\";\n"

renderExpression ::
  Backend ->
  ResolvedTypeDeps ->
  Expr Name MonoType ->
  BackendM MonoType (Text, [TS.TSImport])
renderExpression be dataTypes expr = do
  let readerState = TS.TSReaderState (makeTypeDepMap dataTypes)
   in case TS.fromExpr readerState expr of
        Right (ts, stdlibFuncs) -> case be of
          Typescript -> pure (TS.printModule ts, stdlibFuncs)
          ESModulesJS -> pure (JS.printModule ts, stdlibFuncs)
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
renderImport' ESModulesJS (name, hash') =
  "import { main as "
    <> coerce name
    <> " } from \"./"
    <> moduleFilename ESModulesJS hash'
    <> "\";\n"

renderTypeImport' :: Backend -> (TyCon, ExprHash) -> Text
renderTypeImport' Typescript (typeName, hash') =
  "import * as "
    <> coerce typeName
    <> " from \"./"
    <> moduleFilename Typescript hash'
    <> "\";\n"
renderTypeImport' ESModulesJS (typeName, hash') =
  "import * as "
    <> coerce typeName
    <> " from \"./"
    <> moduleFilename ESModulesJS hash'
    <> "\";\n"

renderTypeSignature' :: MonoType -> Text
renderTypeSignature' mt =
  "/* \n" <> prettyPrint mt <> "\n */"

renderNewline' :: Backend -> Text
renderNewline' _ = "\n"
