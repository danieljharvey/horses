{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wno-orphans #-}

module Repl
  ( repl,
  )
where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified Error.Diagnose as Diag
import Error.Diagnose.Compat.Megaparsec
import qualified Smol.Backend.Compile.RunLLVM as Run
import Smol.Backend.IR.FromExpr.Expr
import Smol.Backend.IR.ToLLVM.ToLLVM
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.ModuleError
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
import Smol.Core.Parser (parseModule)
import System.Console.Haskeline
import Text.Megaparsec

type ParseErrorType = ParseErrorBundle Text Void

replFilename :: FilePath
replFilename = "repl"

instance HasHints Void msg where
  hints _ = mempty

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
              printDiagnostic (fromErrorBundle bundle input) >> loop
            Right moduleParts -> do
              case moduleFromModuleParts mempty moduleParts >>= resolveModuleDeps of
                Left e -> printDiagnostic (moduleErrorDiagnostic e) >> loop
                Right (myModule, deps) -> do
                  case typecheckModule mempty (T.pack input) myModule deps of
                    Left e -> printDiagnostic (moduleErrorDiagnostic e) >> loop
                    Right tcModule -> do
                      let llvmIR = irToLLVM (irFromModule tcModule)
                      resp <- liftIO $ fmap Run.rrResult (Run.run [] llvmIR)
                      liftIO $ putStrLn (T.unpack resp)
                      loop

printDiagnostic :: (MonadIO m) => Diag.Diagnostic Text -> m ()
printDiagnostic =
  Diag.printDiagnostic
    Diag.stderr
    True
    True
    4
    Diag.defaultStyle

-- | turn Megaparsec error + input into a Diagnostic
fromErrorBundle :: ParseErrorType -> String -> Diag.Diagnostic Text
fromErrorBundle bundle input =
  let diag =
        errorDiagnosticFromBundle
          Nothing
          "Parse error on input"
          Nothing
          bundle
   in Diag.addFile diag replFilename input
