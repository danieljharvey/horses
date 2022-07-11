{-# LANGUAGE OverloadedStrings #-}

module Check.Main
  ( check,
  )
where

import Control.Monad.Except
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Mimsa.Actions.Modules.Check as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store.RootPath
import ReplNew.Helpers
import ReplNew.ReplM
import ReplNew.Types
import qualified Shared.LoadProject as Shared
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

  maybeProject <- Shared.loadProject
  -- use project if we're in one, if not, stdlib
  let project = fromRight stdlib maybeProject
  -- check module
  case Actions.run project (Actions.checkModule (prjModuleStore project) fileContents) of
    Right (_, _, (mod', _)) -> do
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
