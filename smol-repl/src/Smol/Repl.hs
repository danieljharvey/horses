{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Repl
  ( repl,
  )
where

import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Smol.Backend.Compile.RunLLVM as Run
import Smol.Backend.IR.FromExpr.Expr
import Smol.Backend.IR.ToLLVM.ToLLVM
import Smol.Modules.Check
import Smol.Modules.Parser (parseModule)
import Smol.Modules.RunTests
import Smol.Modules.Types.ModuleError
import Smol.Repl.Helpers.Diagnostics
import Smol.Repl.Helpers.ShowTestResults
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
            Right moduleParts ->
              case checkModule (T.pack input) moduleParts of
                Left e -> printDiagnostic (moduleErrorDiagnostic (T.pack input) e) >> loop
                Right tcModule -> do
                  liftIO $ printTestResults (runTests tcModule)
                  let llvmIR = irToLLVM (irFromModule tcModule)
                  resp <- liftIO $ fmap Run.rrResult (Run.run [] llvmIR)
                  liftIO $ putStrLn (T.unpack resp)
                  loop
