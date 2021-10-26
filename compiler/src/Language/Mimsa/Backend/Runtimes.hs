{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Backend.Runtimes
  ( Runtime (..),
    RuntimeName (..),
    cjsExportRuntime,
    ejsExportRuntime,
    tsExportRuntime,
    cjsConsoleRuntime,
    tsConsoleRuntime,
    replRuntime,
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
import Data.FileEmbed
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.OpenApi
import Data.Text (Text)
import qualified Data.Text.Encoding as T
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

-- these are saved in a file that is included in compilation
replCode :: Text
replCode =
  T.decodeUtf8 $(embedFile "static/runtimes/commonjs/repl.js")

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

mtVar :: Text -> MonoType
mtVar t = MTVar mempty (TVName (TyVar t))

mtString :: MonoType
mtString = MTPrim mempty MTString

cjsExportRuntime :: Runtime Text
cjsExportRuntime =
  Runtime
    { rtName = RuntimeName "export",
      rtDescription = "Exports the expression",
      rtMonoType = mtVar "any",
      rtBackend = CommonJS,
      rtCode = "module.exports = { main };"
    }

ejsExportRuntime :: Runtime Text
ejsExportRuntime =
  cjsExportRuntime
    { rtBackend = ESModulesJS,
      rtName = RuntimeName "export-ejs",
      rtCode = "export { main }"
    }

tsExportRuntime :: Runtime Text
tsExportRuntime =
  cjsExportRuntime
    { rtBackend = Typescript,
      rtName = RuntimeName "export-ts",
      rtCode = "export { main }"
    }

cjsConsoleRuntime :: Runtime Text
cjsConsoleRuntime =
  Runtime
    { rtName = RuntimeName "console",
      rtDescription = "Logs a string expression to console",
      rtMonoType = MTVar mempty (TVName "a"),
      rtBackend = CommonJS,
      rtCode = "console.log(main);"
    }

ejsConsoleRuntime :: Runtime Text
ejsConsoleRuntime =
  cjsConsoleRuntime
    { rtBackend = ESModulesJS,
      rtName = RuntimeName "console-ejs"
    }

tsConsoleRuntime :: Runtime Text
tsConsoleRuntime =
  cjsConsoleRuntime
    { rtBackend = Typescript,
      rtName = RuntimeName "console-ts"
    }

replRuntime :: Runtime Text
replRuntime =
  Runtime
    { rtName = RuntimeName "repl",
      rtDescription = "Runs a stateful repl session",
      rtMonoType =
        MTRecord
          mempty
          ( M.fromList
              [ ("prompt", mtString),
                ("intro", mtString),
                ("init", mtVar "A"),
                ( "next",
                  MTFunction
                    mempty
                    (mtVar "A")
                    ( MTFunction
                        mempty
                        mtString
                        ( MTPair
                            mempty
                            (mtVar "A")
                            mtString
                        )
                    )
                )
              ]
          ),
      rtBackend = CommonJS,
      rtCode = replCode
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
      [ cjsConsoleRuntime,
        ejsConsoleRuntime,
        cjsExportRuntime,
        ejsExportRuntime,
        replRuntime,
        tsExportRuntime
      ]

getValidRuntimes :: MonoType -> Map RuntimeName (Runtime Text)
getValidRuntimes mt =
  M.filter (\rt -> isRight $ runtimeIsValid rt mt) runtimes

--------

outputIndexFile :: Backend -> Runtime Text -> ExprHash -> Text
outputIndexFile be runtime exprHash =
  let link = case be of
        CommonJS ->
          ("const main = require('./" <> moduleFilename be exprHash <> "').main;\n")
        ESModulesJS ->
          ("import { main } from './" <> moduleFilename be exprHash <> "';\n")
        Typescript ->
          ("import { main } from './" <> moduleFilename be exprHash <> "';\n")
   in link <> rtCode runtime

indexFilename :: Runtime code -> ExprHash -> Text
indexFilename runtime hash' =
  case rtBackend runtime of
    CommonJS ->
      ( "index-" <> coerce (rtName runtime) <> "-"
          <> prettyPrint hash'
          <> ".js"
      )
    ESModulesJS ->
      ( "index-" <> coerce (rtName runtime) <> "-"
          <> prettyPrint hash'
          <> ".mjs"
      )
    Typescript ->
      ( "index-" <> coerce (rtName runtime) <> "-"
          <> prettyPrint hash'
          <> ".ts"
      )
