{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Runtimes
  ( outputIndexFile,
    indexFilename,
  )
where

import Data.Text (Text)
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.Printer
import Language.Mimsa.Types.Store

outputIndexFile :: Backend -> ExprHash -> Text
outputIndexFile be exprHash =
  let importLine = case be of
        ESModulesJS ->
          "import { main as mainExpression } from './" <> moduleFilename be exprHash <> "';\n"
        Typescript ->
          "import { main as mainExpression } from './" <> moduleFilename be exprHash <> "';\n"
   in importLine <> "export const main = mainExpression;\n"

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
