{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Backend.Javascript
  ( renderWithFunction,
  )
where

import Control.Monad.Except
import Data.Coerce
import Data.Map (Map)
import Data.Text (Text)
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import qualified Language.Mimsa.Backend.Typescript.FromExpr as TS
import qualified Language.Mimsa.Backend.Typescript.Monad as TS
import qualified Language.Mimsa.Backend.Typescript.Printer as TS
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
  ( DataType (..),
    Expr (..),
  )
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

renderWithFunction ::
  Backend ->
  ResolvedTypeDeps ->
  Name ->
  Expr Name MonoType ->
  BackendM MonoType Text
renderWithFunction be dataTypes _name expr =
  let readerState = TS.TSReaderState (makeTypeDepMap dataTypes)
   in case be of
        Typescript -> case TS.fromExpr readerState expr of
          Right ts -> pure (TS.printModule ts)
          Left e -> throwError e
        _ -> error "deleted js because yolo"

makeTypeDepMap :: ResolvedTypeDeps -> Map TyCon TyCon
makeTypeDepMap (ResolvedTypeDeps rtd) =
  (\(_, DataType typeName _ _) -> typeName) <$> rtd

_commonJSRenderer ::
  ResolvedTypeDeps ->
  Renderer MonoType Text
_commonJSRenderer dts =
  Renderer
    { renderFunc = renderWithFunction CommonJS dts,
      renderImport = \(name, hash') ->
        pure $
          "const "
            <> coerce name
            <> " = require(\"./"
            <> moduleFilename CommonJS hash'
            <> "\").main;\n",
      renderTypeImport = const (pure ""),
      renderExport = pure . outputExport CommonJS,
      renderStdLib =
        let filename = stdLibFilename CommonJS
         in pure $ "const { __eq, __concat, __patternMatch } = require(\"./" <> filename <> "\");\n",
      renderTypeSignature = \mt -> pure ("/* \n" <> prettyPrint mt <> "\n */"),
      renderNewline = "\n"
    }

_esModulesRenderer ::
  ResolvedTypeDeps ->
  Renderer MonoType Text
_esModulesRenderer dts =
  Renderer
    { renderFunc = renderWithFunction ESModulesJS dts,
      renderImport = \(name, hash') ->
        pure $
          "import { main as "
            <> coerce name
            <> " } from \"./"
            <> moduleFilename ESModulesJS hash'
            <> "\";\n",
      renderExport = pure . outputExport ESModulesJS,
      renderTypeImport = const (pure ""),
      renderStdLib =
        let filename = stdLibFilename ESModulesJS
         in pure $ "import { __eq, __concat, __patternMatch } from \"./" <> filename <> "\";\n",
      renderTypeSignature = \mt -> pure ("/* \n" <> prettyPrint mt <> "\n */"),
      renderNewline = "\n"
    }
