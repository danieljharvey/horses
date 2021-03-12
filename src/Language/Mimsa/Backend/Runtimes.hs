{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Runtimes
  ( Runtime (..),
    RuntimeName (..),
    exportRuntime,
    consoleRuntime,
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
import Data.Map (Map)
import qualified Data.Map as M
import Data.Swagger
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.Printer
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

exportRuntime :: Runtime Javascript
exportRuntime =
  Runtime
    { rtName = RuntimeName "export",
      rtDescription = "Exports the expression",
      rtMonoType = MTVar mempty (TVName (TyVar "any")),
      rtBackend = CommonJS,
      rtCode = "module.exports = { main };"
    }

consoleRuntime :: Runtime Javascript
consoleRuntime =
  Runtime
    { rtName = RuntimeName "console",
      rtDescription = "Logs a string expression to console",
      rtMonoType = MTPrim mempty MTString,
      rtBackend = CommonJS,
      rtCode = "console.log(main);"
    }

runtimeIsValid :: Runtime a -> MonoType -> Either TypeError ()
runtimeIsValid runtime mt =
  runTcMonad
    mempty
    $ unify (rtMonoType runtime) mt >> pure ()

--------

runtimes :: Map RuntimeName (Runtime Javascript)
runtimes =
  M.fromList $
    foldr
      ( \rt as ->
          as <> [(rtName rt, rt)]
      )
      mempty
      [consoleRuntime, exportRuntime]

getValidRuntimes :: MonoType -> Map RuntimeName (Runtime Javascript)
getValidRuntimes mt =
  M.filter (\rt -> isRight $ runtimeIsValid rt mt) runtimes

--------

bsFromText :: Text -> LBS.ByteString
bsFromText = LB.fromChunks . return . T.encodeUtf8

outputIndexFile :: Runtime Javascript -> ExprHash -> Javascript
outputIndexFile runtime exprHash =
  Javascript ("const main = require('./" <> moduleFilename CommonJS exprHash <> "').main;\n")
    <> rtCode runtime

indexFilename :: Runtime code -> ExprHash -> LBS.ByteString
indexFilename runtime hash' =
  case rtBackend runtime of
    CommonJS ->
      bsFromText
        ( "index-" <> coerce (rtName runtime) <> "-"
            <> prettyPrint hash'
            <> ".js"
        )
