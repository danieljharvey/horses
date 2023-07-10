module Smol.Check
  ( check,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Smol.Core.Modules.Check
import Smol.Core.Modules.RunTests
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Parser (parseModule)
import Smol.Repl.Helpers.Diagnostics
import Smol.Repl.Helpers.ShowTestResults
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
      case checkModule input moduleParts of
        Left e ->
          printDiagnostic (moduleErrorDiagnostic e)
            >> pure (ExitFailure 1)
        Right tcModule -> do
          let testResults = runTests tcModule
          liftIO $ printTestResults testResults
          if testsAllPass testResults
            then putStrLn "Great job!" >> pure ExitSuccess
            else pure (ExitFailure 1)

check :: Text -> IO ()
check filePath = do
  liftIO $ checkFile filePath >>= exitWith
