{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Compile.Main
  ( compile,
  )
where

import Control.Monad.Except
import Control.Monad.Logger
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.Types
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Store.RootPath
import Repl.Actions.Compile
import Repl.Persistence
import Repl.ReplM
import Repl.Types
import System.Directory

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

compile :: Backend -> Bool -> IO ()
compile be showLogs' = do
  cfg <- createReplConfig showLogs'
  let action = do
        project <- getProject
        doCompileProject project be
  _ <- runReplM cfg action
  pure ()
