{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Output
  ( outputStoreExpression,
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
import Data.Functor
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Backend.Javascript.Printer as JS
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import qualified Language.Mimsa.Backend.Typescript.FromExpr as TS
import qualified Language.Mimsa.Backend.Typescript.Monad as TS
import Language.Mimsa.Backend.Typescript.Printer
import qualified Language.Mimsa.Backend.Typescript.Printer as TS
import Language.Mimsa.Backend.Typescript.Types
import qualified Language.Mimsa.Backend.Typescript.Types as TS
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Store.ResolveDataTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Utils

-- returns [Maybe, hash], [These, hash], [Either, hash] - used for imports
typeBindingsByType :: Store a -> Map (Maybe ModuleName, TyCon) ExprHash -> Map TypeName ExprHash
typeBindingsByType store tb =
  let getTypeName' exprHash =
        case lookupExprHashFromStore store exprHash of
          Just se -> storeExprToDataTypes se $> exprHash
          Nothing -> mempty
   in stripModules $ mconcat (getTypeName' <$> M.elems tb)

-- remove moduleName from type. will probably need these later when we come to
-- fix TS but for now YOLO
stripModules :: (Ord b) => Map (a, b) c -> Map b c
stripModules = M.fromList . fmap (first snd) . M.toList

-- | Numbers each infix operator, and names them `_infix0`, `_infix1` etc
-- these are then used to create both imports and the mapping from infix
-- operator to the variable to use in the TS code
nameInfixes :: Map InfixOp ExprHash -> (Map Name ExprHash, Map InfixOp TSName)
nameInfixes infixes =
  let numbered = addNumbersToMap infixes
      infixName i = Name $ "_infix" <> T.pack (show i)
      tsInfixName i = TSName $ "_infix" <> T.pack (show i)
      toNameToHash = M.fromList . fmap (first infixName) . M.elems
      toInfixToName = tsInfixName . fst <$> numbered
   in (toNameToHash numbered, toInfixToName)

-- | Need to also include any types mentioned but perhaps not explicitly used
outputStoreExpression ::
  Backend ->
  ResolvedTypeDeps ->
  Store any ->
  MonoType ->
  StoreExpression MonoType ->
  BackendM MonoType Text
outputStoreExpression be dataTypes store mt se = do
  let typeBindings = typeBindingsByType store (storeTypeBindings se)

      (infixHashes, infixNames) =
        nameInfixes (storeInfixes se)

      deps =
        renderImport' be
          <$> M.toList (storeBindings se)

      typeDeps =
        renderTypeImport' be
          <$> M.toList typeBindings

      infixDeps =
        renderInfixImport be
          <$> M.toList infixHashes

      -- we import types where they are used transitively, so we don't need
      -- them if they are imported explicitly
      requiredTypeImports =
        M.filterWithKey (\(_, tn) _ -> S.notMember tn (M.keysSet typeBindings)) (storeTypes se)

      directTypeDeps = renderDirectTypeImport be <$> M.toList requiredTypeImports

  (func, stdlibFuncs) <-
    renderExpression be dataTypes infixNames (storeExpression se)

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
  ResolvedTypeDeps ->
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

-- map of `Just` -> `Maybe`, `Nothing` -> `Maybe`..
makeTypeDepMap :: ResolvedTypeDeps -> Map TyCon TypeName
makeTypeDepMap (ResolvedTypeDeps rtd) =
  (\(_, DataType typeName _ _) -> typeName) <$> first snd rtd

renderImport' :: Backend -> ((a, Name), ExprHash) -> Text
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

renderInfixImport :: Backend -> (Name, ExprHash) -> Text
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

renderTypeImport' :: Backend -> (TypeName, ExprHash) -> Text
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
renderDirectTypeImport :: Backend -> ((Maybe ModuleName, TypeName), ExprHash) -> Text
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

outputIndexFile :: Backend -> Map Name ExprHash -> Map ModuleName ModuleHash -> Text
outputIndexFile be exportMap exportModuleMap =
  let exportExpression (name, exprHash) = case be of
        ESModulesJS ->
          "export { main as " <> printTSName (coerce name) <> " } from './"
            <> storeExprFilename be exprHash
            <> fileExtension be
            <> "';"
        Typescript ->
          "export { main as " <> printTSName (coerce name) <> " } from './"
            <> storeExprFilename be exprHash
            <> "';"

      exportModule (modName, modHash) = case be of
        ESModulesJS ->
          "export { * as " <> printTSName (coerce modName) <> " } from './"
            <> moduleImport be modHash
            <> "';"
        Typescript ->
          "export { main as " <> printTSName (coerce modName) <> " } from './"
            <> moduleImport be modHash
            <> "';"

      allExports =
        (exportExpression <$> M.toList exportMap)
          <> (exportModule <$> M.toList exportModuleMap)
   in T.intercalate "\n" allExports

-- | file name of index file (no extension for ts)
indexImport :: Backend -> ExprHash -> Text
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
indexFilename :: Backend -> ExprHash -> Text
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
