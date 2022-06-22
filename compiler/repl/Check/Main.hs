{-# LANGUAGE OverloadedStrings #-}

module Check.Main
  ( check,
  )
where

import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Modules.Check
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Store.RootPath
import ReplNew.Helpers
import ReplNew.ReplM
import ReplNew.Types
import System.Directory
import System.Exit
import Prelude hiding (init)

createReplConfig :: (MonadIO m) => Bool -> m ReplConfig
createReplConfig showLogs' = do
  path <- liftIO getCurrentDirectory
  pure $ ReplConfig (RootPath path) showLogs'

-- read a file, check if it is OK etc
checkFile :: Text -> ReplM (Error Annotation) ExitCode
checkFile filePath = do
  replOutput ("Reading " <> T.pack (show filePath))
  fileContents <- liftIO $ T.readFile (T.unpack filePath)
  -- TODO: currently we pass no available modules
  -- but we should really read the current project
  -- and then include those so that external deps are available?
  case checkModule fileContents mempty of
    Right (mod', _) -> do
      liftIO $ T.putStrLn $ prettyPrint mod'
      -- format and rewrite
      -- liftIO $ T.writeFile (T.unpack filePath) (prettyPrint mod')
      pure ExitSuccess
    Left err -> do
      outputErrorAsDiagnostic err
      pure (ExitFailure 1)

check :: Bool -> Text -> IO ()
check showLogs' filePath = do
  cfg <- createReplConfig showLogs'
  exitCode <- runReplM cfg (checkFile filePath)
  case exitCode of
    Right ec -> exitWith ec
    _ -> exitWith $ ExitFailure 1
