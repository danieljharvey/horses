{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Output
  ( renderDataTypeWithDeps,
    renderExprWithDeps,
    outputIndexFile,
    indexFilename,
    indexImport,
    projectIndexFilename,
    moduleFilename,
    moduleImport,
  )
where

import Control.Monad.Except
import Data.Bifunctor
import Data.Coerce
import Data.List (intersperse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Backend.Javascript.Printer as JS
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import qualified Language.Mimsa.Backend.Typescript.FromDataType as TS
import qualified Language.Mimsa.Backend.Typescript.FromExpr as TS
import qualified Language.Mimsa.Backend.Typescript.Monad as TS
import Language.Mimsa.Backend.Typescript.Printer
import qualified Language.Mimsa.Backend.Typescript.Printer as TS
import Language.Mimsa.Backend.Typescript.Types
import qualified Language.Mimsa.Backend.Typescript.Types as TS
import Language.Mimsa.Core

-- | Numbers each infix operator, and names them `_infix0`, `_infix1` etc
-- these are then used to create both imports and the mapping from infix
-- operator to the variable to use in the TS code
nameInfixes :: Map InfixOp hash -> (Map Name hash, Map InfixOp TSName)
nameInfixes infixes =
  let numbered = addNumbersToMap infixes
      infixName i = Name $ "_infix" <> T.pack (show i)
      tsInfixName i = TSName $ "_infix" <> T.pack (show i)
      toNameToHash = M.fromList . fmap (first infixName) . M.elems
      toInfixToName = tsInfixName . fst <$> numbered
   in (toNameToHash numbered, toInfixToName)

-- | Need to also include any types mentioned but perhaps not explicitly used
renderExprWithDeps ::
  (Printer hash) =>
  Backend ->
  Map (Maybe ModuleName, TyCon) DataType ->
  Map TypeName hash ->
  Map InfixOp hash ->
  Map (Maybe ModuleName, Name) hash ->
  Map (Maybe ModuleName, TypeName) hash ->
  Expr Name MonoType ->
  BackendM MonoType Text
renderExprWithDeps be dataTypes typeBindings infixes bindings types expr = do
  let (infixHashes, infixNames) =
        nameInfixes infixes

      deps =
        renderImport' be
          <$> M.toList bindings

      typeDeps =
        renderTypeImport' be
          <$> M.toList typeBindings

      infixDeps =
        renderInfixImport be
          <$> M.toList infixHashes

      mt = getAnnotation expr
      -- we import types where they are used transitively, so we don't need
      -- them if they are imported explicitly
      requiredTypeImports =
        M.filterWithKey (\(_, tn) _ -> S.notMember tn (M.keysSet typeBindings)) types

      directTypeDeps = renderDirectTypeImport be <$> M.toList requiredTypeImports

  (func, stdlibFuncs) <-
    renderExpression be dataTypes infixNames expr

  let stdlib = stdlibImport be stdlibFuncs
      typeComment = renderTypeSignature' mt

  pure $
    mconcat
      ( intersperse
          (renderNewline' be)
          [ mconcat deps,
            mconcat typeDeps,
            mconcat infixDeps,
            mconcat directTypeDeps,
            stdlib,
            typeComment,
            func
          ]
      )

renderDataTypeWithDeps ::
  (Printer hash) =>
  Backend ->
  Map (Maybe ModuleName, TyCon) DataType ->
  DataType ->
  Map (Maybe ModuleName, TypeName) hash ->
  BackendM MonoType Text
renderDataTypeWithDeps be dataTypes dt types = do
  let directTypeDeps = renderDirectTypeImport be <$> M.toList types

  prettyDataType <- renderDataType be dataTypes dt

  pure $
    mconcat
      ( intersperse
          (renderNewline' be)
          [ mconcat directTypeDeps,
            prettyDataType
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
   in "import { "
        <> T.intercalate ", " filteredNames
        <> " } from \"./"
        <> stdlibFilename backend
        <> "\";\n"

renderExpression ::
  Backend ->
  Map (Maybe ModuleName, TyCon) DataType ->
  Map InfixOp TSName ->
  Expr Name MonoType ->
  BackendM MonoType (Text, [TS.TSImport])
renderExpression be dataTypes infixes expr = do
  let readerState =
        TS.TSReaderState
          (makeTypeDepMap dataTypes)
          infixes
      startState = TS.TSCodegenState mempty mempty mempty
   in case TS.fromExpr readerState startState expr of
        Right (ts, stdlibFuncs) -> case be of
          Typescript -> pure (TS.printModule ts, stdlibFuncs)
          ESModulesJS -> pure (JS.printModule ts, stdlibFuncs)
        Left e -> throwError e

renderDataType ::
  Backend ->
  Map (Maybe ModuleName, TyCon) DataType ->
  DataType ->
  BackendM MonoType Text
renderDataType be dataTypes dt = do
  let readerState =
        TS.TSReaderState
          (makeTypeDepMap dataTypes)
          mempty
      startState = TS.TSCodegenState mempty mempty mempty
   in case TS.fromDataType readerState startState dt of
        Right tsDt -> case be of
          Typescript -> pure $ TS.printDataType tsDt
          ESModulesJS -> pure $ JS.printDataType tsDt
        Left e -> throwError e

-- map of `Just` -> `Maybe`, `Nothing` -> `Maybe`..
makeTypeDepMap ::
  Map (Maybe ModuleName, TyCon) DataType ->
  Map TyCon TypeName
makeTypeDepMap rtd =
  (\(DataType typeName _ _) -> typeName) <$> first snd rtd

renderImport' :: (Printer hash) => Backend -> ((a, Name), hash) -> Text
renderImport' Typescript ((_, name), hash') =
  "import { main as "
    <> printTSName (coerce name)
    <> " } from \"./"
    <> storeExprFilename Typescript hash'
    <> "\";\n"
renderImport' ESModulesJS ((_, name), hash') =
  "import { main as "
    <> printTSName (coerce name)
    <> " } from \"./"
    <> storeExprFilename ESModulesJS hash'
    <> "\";\n"

renderInfixImport :: (Printer hash) => Backend -> (Name, hash) -> Text
renderInfixImport Typescript (name, hash') =
  "import { main as "
    <> printTSName (coerce name)
    <> " } from \"./"
    <> storeExprFilename Typescript hash'
    <> "\";\n"
renderInfixImport ESModulesJS (name, hash') =
  "import { main as "
    <> printTSName (coerce name)
    <> " } from \"./"
    <> storeExprFilename ESModulesJS hash'
    <> "\";\n"

renderTypeImport' :: (Printer hash) => Backend -> (TypeName, hash) -> Text
renderTypeImport' Typescript (typeName, hash') =
  "import * as "
    <> coerce typeName
    <> " from \"./"
    <> storeExprFilename Typescript hash'
    <> "\";\n"
renderTypeImport' ESModulesJS (typeName, hash') =
  "import * as "
    <> coerce typeName
    <> " from \"./"
    <> storeExprFilename ESModulesJS hash'
    <> "\";\n"

-- | 10x-ing hard right now, could be rekt
renderDirectTypeImport :: (Printer hash) => Backend -> ((Maybe ModuleName, TypeName), hash) -> Text
renderDirectTypeImport Typescript ((_, typeName), hash') =
  "import type { "
    <> coerce typeName
    <> " } from \"./"
    <> storeExprFilename Typescript hash'
    <> "\";\n"
renderDirectTypeImport ESModulesJS _ = mempty

renderTypeSignature' :: MonoType -> Text
renderTypeSignature' mt =
  "/* \n" <> prettyPrint mt <> "\n */"

renderNewline' :: Backend -> Text
renderNewline' _ = "\n"

outputIndexFile ::
  (Printer hash) =>
  Backend ->
  Map Name hash ->
  Map ModuleName ModuleHash ->
  Map TypeName hash ->
  Text
outputIndexFile be exportMap exportModuleMap exportTypeMap =
  let exportExpression (name, exprHash) = case be of
        ESModulesJS ->
          "export { main as "
            <> printTSName (coerce name)
            <> " } from './"
            <> storeExprFilename be exprHash
            <> fileExtension be
            <> "';"
        Typescript ->
          "export { main as "
            <> printTSName (coerce name)
            <> " } from './"
            <> storeExprFilename be exprHash
            <> "';"

      exportModule (modName, modHash) = case be of
        ESModulesJS ->
          "export * as "
            <> printTSName (coerce modName)
            <> " from './"
            <> moduleImport be modHash
            <> "';"
        Typescript ->
          "export * as "
            <> printTSName (coerce modName)
            <> " from './"
            <> moduleImport be modHash
            <> "';"

      exportType (_typeName, exprHash) = case be of
        ESModulesJS ->
          "export * from './"
            <> storeExprFilename be exprHash
            <> fileExtension be
            <> "';"
        Typescript ->
          "export * from './"
            <> storeExprFilename be exprHash
            <> "';"

      allExports =
        (exportExpression <$> M.toList exportMap)
          <> (exportModule <$> M.toList exportModuleMap)
          <> (exportType <$> M.toList exportTypeMap)
   in T.intercalate "\n" allExports

-- | file name of index file (no extension for ts)
indexImport :: (Printer hash) => Backend -> hash -> Text
indexImport be hash' =
  case be of
    ESModulesJS ->
      "index-"
        <> prettyPrint hash'
        <> ".mjs"
    Typescript ->
      "index-"
        <> prettyPrint hash'

-- | filename of index file (including extension always)
indexFilename :: (Printer hash) => Backend -> hash -> Text
indexFilename be hash' =
  case be of
    ESModulesJS ->
      "index-"
        <> prettyPrint hash'
        <> ".mjs"
    Typescript ->
      "index-"
        <> prettyPrint hash'
        <> ".ts"

projectIndexFilename :: Backend -> Text
projectIndexFilename be =
  case be of
    ESModulesJS ->
      "index.mjs"
    Typescript ->
      "index.ts"

-- | filename for a module (without extension for TS)
moduleImport :: Backend -> ModuleHash -> Text
moduleImport be modHash =
  case be of
    ESModulesJS ->
      "module-" <> prettyPrint modHash <> ".mjs"
    Typescript ->
      "module-" <> prettyPrint modHash

-- | filename of module, always including extension
moduleFilename :: Backend -> ModuleHash -> Text
moduleFilename be modHash =
  case be of
    ESModulesJS ->
      moduleImport be modHash
    Typescript ->
      moduleImport be modHash <> ".ts"
