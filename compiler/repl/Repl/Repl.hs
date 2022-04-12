{-# LANGUAGE OverloadedStrings #-}

module Repl.Repl
  ( repl,
  )
where

import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Parser
import Language.Mimsa.Project.Stdlib
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
    `catchError` \_ -> do
      logError "Failed to load project, loading default project"
      pure stdlib

repl :: Bool -> IO ()
repl showLogs' = do
  cfg <- createReplConfig showLogs'
  _ <- runReplM cfg replM
  pure ()

replM :: ReplM (Error Annotation) ()
replM = do
  env <- getProject
  runInputT defaultSettings (loop env)
  where
    loop ::
      Project Annotation ->
      InputT (ReplM (Error Annotation)) ()
    loop exprs' = do
      minput <- getInputLine ":> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          newEnv <- lift $ parseCommand exprs' (T.pack input)
          loop newEnv

parseCommand ::
  Project Annotation ->
  Text ->
  ReplM (Error Annotation) (Project Annotation)
parseCommand env input =
  case parseAndFormat replParser input of
    Left e -> do
      replOutput e
      pure env
    Right replAction -> do
      newExprs <- doReplAction env input replAction
      _ <- mapError StoreErr (saveProject newExprs)
      pure newExprs
