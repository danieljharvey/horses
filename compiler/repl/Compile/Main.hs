{-# LANGUAGE OverloadedStrings #-}

module Compile.Main
  ( compile,
  )
where

import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Parser
import Language.Mimsa.Printer
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Store.Storage
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Store.RootPath
import Repl.Actions (doReplAction)
import Repl.Parser (replParser)
import Repl.Persistence
import Repl.ReplM
import Repl.Types
import System.Console.Haskeline
import System.Directory
import Prelude hiding (init)

createReplConfig :: (MonadIO m) => Bool -> m ReplConfig
createReplConfig showLogs' = do
  path <- liftIO getCurrentDirectory
  pure $ ReplConfig (RootPath path) showLogs'

getProject :: ReplM (Error Annotation) (Project Annotation)
getProject =
  do
    env <- mapError StoreErr loadProject
    let items = length . getStore . prjStore $ env
    replOutput $ "Successfully loaded project, " <> T.pack (show items) <> " store items found"
    pure env
    `catchError` \e -> do
      logDebugN (prettyPrint e)
      replOutput @Text "Failed to load project, have you initialised a project in this folder?"
      throwError e

init :: Bool -> IO ()
init showLogs' = do
  cfg <- createReplConfig showLogs'
  _ <- runReplM cfg do
    project <- getProject
    doCompileProject be project
  pure ()
