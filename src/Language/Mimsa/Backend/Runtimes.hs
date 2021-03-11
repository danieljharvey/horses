{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Runtimes
  ( Runtime (..),
    exportRuntime,
    consoleRuntime,
    runtimeIsValid,
    outputIndexFile,
  )
where

import Data.Text (Text)
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Typechecker.Unify
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

data Runtime code = Runtime
  { rtName :: Text,
    rtDescription :: Text,
    rtMonoType :: MonoType,
    rtBackend :: Backend,
    rtCode :: code
  }

exportRuntime :: Runtime Javascript
exportRuntime =
  Runtime
    { rtName = "Export",
      rtDescription = "Exports the expression",
      rtMonoType = MTVar mempty (TVName (TyVar "any")),
      rtBackend = CommonJS,
      rtCode = "module.exports = { main };"
    }

consoleRuntime :: Runtime Javascript
consoleRuntime =
  Runtime
    { rtName = "Console.log",
      rtDescription = "Logs the expression to console",
      rtMonoType = MTPrim mempty MTString,
      rtBackend = CommonJS,
      rtCode = "console.log(main);"
    }

runtimeIsValid :: Runtime a -> MonoType -> Either TypeError ()
runtimeIsValid runtime mt =
  runTcMonad
    mempty
    $ unify (rtMonoType runtime) mt >> pure ()

outputIndexFile :: Runtime Javascript -> ExprHash -> Javascript
outputIndexFile runtime exprHash =
  Javascript ("const main = require('./" <> moduleFilename CommonJS exprHash <> "').main;\n")
    <> rtCode runtime
