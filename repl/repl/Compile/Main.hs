{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Compile.Main
  ( compile,
  )
where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Compile as Actions
import Language.Mimsa.Backend.Types
import Language.Mimsa.Core
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Store.RootPath
import Repl.Helpers
import Repl.ReplM
import Repl.Types
import qualified Shared.LoadProject as Shared
import System.Directory
import System.Exit

createReplConfig :: (MonadIO m) => Bool -> m ReplConfig
createReplConfig showLogs' = do
  path <- liftIO getCurrentDirectory
  pure $ ReplConfig (RootPath path) showLogs'

compileProject :: Backend -> ReplM (Error Annotation) ExitCode
compileProject be = do
  maybeProject <- Shared.loadProject
  case maybeProject of
    Right project -> do
      _ <-
        toReplM project (Actions.compileProject be)
      replOutput @Text "Compilation complete!"
      pure ExitSuccess
    Left _e -> do
      replOutput @Text "Failed to load project, have you initialised a project in this folder?"
      pure (ExitFailure 1)

compile :: Backend -> Bool -> IO ()
compile be showLogs' = do
  cfg <- createReplConfig showLogs'
  exitCode <- runReplM cfg (compileProject be)
  case exitCode of
    Right ec -> exitWith ec
    _ -> exitWith $ ExitFailure 1
