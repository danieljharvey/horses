{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Backend.Javascript
  (
  )
where

{-
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
