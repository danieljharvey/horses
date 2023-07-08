{-# LANGUAGE OverloadedStrings #-}

module Smol.Check
  ( check,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
import Smol.Core.Parser (parseModule)
import Smol.Repl.Helpers.Diagnostics
import System.Exit
import Prelude hiding (init)

-- read a file, check if it is OK etc
checkFile :: (MonadIO m) => Text -> m ExitCode
checkFile filePath = liftIO $ do
  putStrLn ("Reading " <> show filePath)
  input <- T.readFile (T.unpack filePath)
  case parseModule input of
    Left bundle -> do
      printDiagnostic (fromErrorBundle bundle input)
        >> pure (ExitFailure 1)
    Right moduleParts -> do
      case moduleFromModuleParts moduleParts >>= resolveModuleDeps of
        Left e ->
          printDiagnostic (moduleErrorDiagnostic e)
            >> pure (ExitFailure 1)
        Right (myModule, deps) -> do
          case typecheckModule input myModule deps of
            Left e ->
              printDiagnostic (moduleErrorDiagnostic e)
                >> pure (ExitFailure 1)
            Right _tcModule -> do
              putStrLn "Great job!"
              pure ExitSuccess

check :: Text -> IO ()
check filePath = do
  liftIO $ checkFile filePath >>= exitWith
