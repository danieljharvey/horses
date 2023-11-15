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
import Smol.Core.Modules.Types.ModuleItem
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Parser (parseModule)
import Smol.Repl.Helpers.Diagnostics
import Smol.Repl.Helpers.ShowTestResults
import System.Exit
import Prelude hiding (init)
import Smol.Core.Modules.PrettyPrint (printModuleParts)
import Smol.Core.Printer

-- read a file, check if it is OK etc
checkFile :: (MonadIO m) => Text -> m ExitCode
checkFile filePath = liftIO $ do
  input <- T.readFile (T.unpack filePath)
  case parseModule input of
    Left bundle -> do
      printDiagnostic (fromErrorBundle bundle input)
        >> pure (ExitFailure 1)
    Right moduleParts -> do
      case checkModule input moduleParts of
        Left e ->
          printDiagnostic (moduleErrorDiagnostic input e)
            >> pure (ExitFailure 1)
        Right tcModule -> do
          let testResults = runTests tcModule
          liftIO $ printTestResults testResults

          -- auto format the file for lols
          format (T.unpack filePath) input moduleParts
          if testsAllPass testResults
            then putStrLn "Great job!" >> pure ExitSuccess
            else pure (ExitFailure 1)

-- format the file, and if it's changed, save it
format :: (MonadIO m) => FilePath -> Text -> [ModuleItem ann] -> m ()
format filePath originalInput moduleItems = do
                  let printed = renderWithWidth 80 $ printModuleParts moduleItems
                  if printed /= originalInput then
                    liftIO $ T.writeFile filePath printed
                    else pure ()


check :: Text -> IO ()
check filePath = do
  liftIO $ checkFile filePath >>= exitWith
