{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Runtimes
  ( Runtime (..),
    RuntimeName (..),
    ejsExportRuntime,
    ejsConsoleRuntime,
    tsExportRuntime,
    tsConsoleRuntime,
    runtimeIsValid,
    outputIndexFile,
    indexFilename,
    getValidRuntimes,
    runtimes,
  )
where

import qualified Data.Aeson as JSON
import Data.Coerce (coerce)
import Data.Either (isRight)
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.OpenApi
import Data.Text (Text)
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.Solve
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

newtype RuntimeName
  = RuntimeName Text
  deriving newtype
    ( Eq,
      Ord,
      Show,
      JSON.ToJSONKey,
      JSON.FromJSON,
      ToSchema
    )

data Runtime code = Runtime
  { rtName :: RuntimeName,
    rtDescription :: Text,
    rtMonoType :: MonoType,
    rtBackend :: Backend,
    rtCode :: code
  }

uniVar :: Int -> MonoType
uniVar i = MTVar mempty (TVUnificationVar i)

ejsExportRuntime :: Runtime Text
ejsExportRuntime =
  Runtime
    { rtName = RuntimeName "export-ejs",
      rtDescription = "Exports the expression",
      rtBackend = ESModulesJS,
      rtCode = "export { main }",
      rtMonoType = uniVar (-1)
    }

tsExportRuntime :: Runtime Text
tsExportRuntime =
  ejsExportRuntime
    { rtBackend = Typescript,
      rtName = RuntimeName "export-ts",
      rtCode = "export { main }"
    }

ejsConsoleRuntime :: Runtime Text
ejsConsoleRuntime =
  Runtime
    { rtDescription = "Logs a string expression to console",
      rtBackend = ESModulesJS,
      rtName = RuntimeName "console-ejs",
      rtCode = "console.log(main);",
      rtMonoType = uniVar (-1)
    }

tsConsoleRuntime :: Runtime Text
tsConsoleRuntime =
  ejsConsoleRuntime
    { rtBackend = Typescript,
      rtName = RuntimeName "console-ts"
    }

runtimeIsValid :: Runtime a -> MonoType -> Either TypeError ()
runtimeIsValid runtime mt =
  runSolveM
    mempty
    defaultTcState
    (unify (rtMonoType runtime) mt >> pure ())
    $> ()

--------

runtimes :: Map RuntimeName (Runtime Text)
runtimes =
  M.fromList $
    foldr
      ( \rt as ->
          as <> [(rtName rt, rt)]
      )
      mempty
      [ ejsConsoleRuntime,
        tsConsoleRuntime,
        ejsExportRuntime,
        tsExportRuntime
      ]

getValidRuntimes :: MonoType -> Map RuntimeName (Runtime Text)
getValidRuntimes mt =
  M.filter (\rt -> isRight $ runtimeIsValid rt mt) runtimes

--------

outputIndexFile :: Backend -> Runtime Text -> ExprHash -> Text
outputIndexFile be runtime exprHash =
  let link = case be of
        ESModulesJS ->
          "import { main } from './" <> moduleFilename be exprHash <> "';\n"
        Typescript ->
          "import { main } from './" <> moduleFilename be exprHash <> "';\n"
   in link <> rtCode runtime

indexFilename :: Runtime code -> ExprHash -> Text
indexFilename runtime hash' =
  case rtBackend runtime of
    ESModulesJS ->
      "index-" <> coerce (rtName runtime) <> "-"
        <> prettyPrint hash'
        <> ".mjs"
    Typescript ->
      "index-" <> coerce (rtName runtime) <> "-"
        <> prettyPrint hash'
        <> ".ts"
