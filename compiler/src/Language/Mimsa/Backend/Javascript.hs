{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Backend.Javascript
  ( outputJavascript,
    renderWithFunction,
    Javascript (..),
  )
where

import Control.Monad.Except
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Coerce
import Data.String
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import qualified Language.Mimsa.Backend.Typescript.FromExpr as TS
import qualified Language.Mimsa.Backend.Typescript.Printer as TS
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
  ( Expr (..),
  )
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

----
newtype Javascript = Javascript LBS.ByteString
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

instance Printer Javascript where
  prettyPrint (Javascript bs) = (T.decodeUtf8 . B.concat . LBS.toChunks) bs

instance IsString Javascript where
  fromString = Javascript . LB.pack

----

textToJS :: Text -> Javascript
textToJS = Javascript . LB.fromChunks . return . T.encodeUtf8

renderWithFunction ::
  Backend ->
  ResolvedTypeDeps ->
  Name ->
  Expr Name MonoType ->
  BackendM MonoType Javascript
renderWithFunction be _dataTypes _name expr =
  case be of
    Typescript -> case TS.fromExpr expr of
      Right ts -> pure (textToJS (TS.printModule ts))
      Left e -> throwError e
    _ -> error "deleted js because yolo"

outputJavascript ::
  Backend ->
  ResolvedTypeDeps ->
  MonoType ->
  StoreExpression MonoType ->
  BackendM MonoType Javascript
outputJavascript be dataTypes =
  outputStoreExpression
    ( case be of
        CommonJS -> commonJSRenderer dataTypes
        ESModulesJS -> esModulesRenderer dataTypes
        Typescript -> tsModulesRenderer dataTypes
    )

commonJSRenderer ::
  ResolvedTypeDeps ->
  Renderer MonoType Javascript
commonJSRenderer dts =
  Renderer
    { renderFunc = renderWithFunction CommonJS dts,
      renderImport = \(name, hash') ->
        pure $
          "const "
            <> textToJS (coerce name)
            <> " = require(\"./"
            <> Javascript (moduleFilename CommonJS hash')
            <> "\").main;\n",
      renderExport = pure . Javascript . outputExport CommonJS,
      renderStdLib =
        let filename = Javascript (stdLibFilename CommonJS)
         in pure $ "const { __eq, __concat, __patternMatch } = require(\"./" <> filename <> "\");\n",
      renderTypeSignature = \mt -> pure ("/* \n" <> textToJS (prettyPrint mt) <> "\n */"),
      renderNewline = textToJS "\n"
    }

esModulesRenderer ::
  ResolvedTypeDeps ->
  Renderer MonoType Javascript
esModulesRenderer dts =
  Renderer
    { renderFunc = renderWithFunction ESModulesJS dts,
      renderImport = \(name, hash') ->
        pure $
          "import { main as "
            <> textToJS (coerce name)
            <> " } from \"./"
            <> Javascript (moduleFilename ESModulesJS hash')
            <> "\";\n",
      renderExport = pure . Javascript . outputExport ESModulesJS,
      renderStdLib =
        let filename = Javascript (stdLibFilename ESModulesJS)
         in pure $ "import { __eq, __concat, __patternMatch } from \"./" <> filename <> "\";\n",
      renderTypeSignature = \mt -> pure ("/* \n" <> textToJS (prettyPrint mt) <> "\n */"),
      renderNewline = textToJS "\n"
    }

tsModulesRenderer ::
  ResolvedTypeDeps ->
  Renderer MonoType Javascript
tsModulesRenderer dts =
  Renderer
    { renderFunc = renderWithFunction Typescript dts,
      renderImport = \(name, hash') ->
        pure $
          "import { main as "
            <> textToJS (coerce name)
            <> " } from \"./"
            <> Javascript (moduleFilename Typescript hash')
            <> "\";\n",
      renderExport =
        pure . Javascript . outputExport Typescript,
      renderStdLib =
        pure "",
      renderTypeSignature =
        \mt -> pure ("/* \n" <> textToJS (prettyPrint mt) <> "\n */"),
      renderNewline = textToJS "\n"
    }
