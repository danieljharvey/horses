{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Backend.Runtimes
  ( Runtime (..),
    RuntimeName (..),
    cjsExportRuntime,
    ejsExportRuntime,
    cjsConsoleRuntime,
    runtimeIsValid,
    outputIndexFile,
    indexFilename,
    getValidRuntimes,
    runtimes,
  )
where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Coerce (coerce)
import Data.Either (isRight)
import Data.FileEmbed
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.OpenApi
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Language.Mimsa.Backend.Javascript
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
replCode :: LBS.ByteString
replCode =
  LBS.fromStrict $(embedFile "static/runtimes/commonjs/repl.js")

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

cjsExportRuntime :: Runtime Javascript
cjsExportRuntime =
  Runtime
    { rtName = RuntimeName "export",
      rtDescription = "Exports the expression",
      rtMonoType = mtVar "any",
      rtBackend = CommonJS,
      rtCode = "module.exports = { main };"
    }

ejsExportRuntime :: Runtime Javascript
ejsExportRuntime =
  cjsExportRuntime
    { rtBackend = ESModulesJS,
      rtName = RuntimeName "export-ejs",
      rtCode = "export { main }"
    }

cjsConsoleRuntime :: Runtime Javascript
cjsConsoleRuntime =
  Runtime
    { rtName = RuntimeName "console",
      rtDescription = "Logs a string expression to console",
      rtMonoType = mtString,
      rtBackend = CommonJS,
      rtCode = "console.log(main);"
    }

ejsConsoleRuntime :: Runtime Javascript
ejsConsoleRuntime =
  cjsConsoleRuntime
    { rtBackend = ESModulesJS,
      rtName = RuntimeName "console-ejs"
    }

replRuntime :: Runtime Javascript
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
      rtCode = Javascript replCode
    }

runtimeIsValid :: Runtime a -> MonoType -> Either TypeError ()
runtimeIsValid runtime mt =
  runSolveM
    mempty
    defaultTcState
    (unify (rtMonoType runtime) mt >> pure ())
    $> ()

--------

runtimes :: Map RuntimeName (Runtime Javascript)
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
        replRuntime
      ]

getValidRuntimes :: MonoType -> Map RuntimeName (Runtime Javascript)
getValidRuntimes mt =
  M.filter (\rt -> isRight $ runtimeIsValid rt mt) runtimes

--------

bsFromText :: Text -> LBS.ByteString
bsFromText = LB.fromChunks . return . T.encodeUtf8

outputIndexFile :: Backend -> Runtime Javascript -> ExprHash -> Javascript
outputIndexFile be runtime exprHash =
  let link = case be of
        CommonJS ->
          Javascript ("const main = require('./" <> moduleFilename be exprHash <> "').main;\n")
        ESModulesJS ->
          Javascript ("import { main } from './" <> moduleFilename be exprHash <> "';\n")
   in link <> rtCode runtime

indexFilename :: Runtime code -> ExprHash -> LBS.ByteString
indexFilename runtime hash' =
  case rtBackend runtime of
    CommonJS ->
      bsFromText
        ( "index-" <> coerce (rtName runtime) <> "-"
            <> prettyPrint hash'
            <> ".js"
        )
    ESModulesJS ->
      bsFromText
        ( "index-" <> coerce (rtName runtime) <> "-"
            <> prettyPrint hash'
            <> ".mjs"
        )
