{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Backend.Javascript
  (
  )
where

{-
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

-}
