{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wno-orphans #-}

module Smol.Repl
  ( repl,
  )
where

import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Smol.Backend.Compile.RunLLVM as Run
import Smol.Backend.IR.FromExpr.Expr
import Smol.Backend.IR.ToLLVM.ToLLVM
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.ModuleError
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
import Smol.Core.Parser (parseModule)
import Smol.Repl.Helpers.Diagnostics
import System.Console.Haskeline

repl :: IO ()
repl = do
  putStrLn "Welcome to smol"
  putStrLn "Exit with :quit"
  runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine ":> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          case parseModule (T.pack input) of
            Left bundle -> do
              printDiagnostic (fromErrorBundle bundle (T.pack input)) >> loop
            Right moduleParts -> do
              case moduleFromModuleParts moduleParts >>= resolveModuleDeps of
                Left e -> printDiagnostic (moduleErrorDiagnostic e) >> loop
                Right (myModule, deps) -> do
                  case typecheckModule (T.pack input) myModule deps of
                    Left e -> printDiagnostic (moduleErrorDiagnostic e) >> loop
                    Right tcModule -> do
                      let llvmIR = irToLLVM (irFromModule tcModule)
                      resp <- liftIO $ fmap Run.rrResult (Run.run [] llvmIR)
                      liftIO $ putStrLn (T.unpack resp)
                      loop
