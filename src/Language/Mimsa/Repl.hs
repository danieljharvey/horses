{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl
  ( repl,
    evaluateText,
  )
where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Language.Mimsa.Monad
import Language.Mimsa.Parser
import Language.Mimsa.Project
  ( defaultProject,
    loadProject,
    saveProject,
  )
import Language.Mimsa.Repl.Actions (doReplAction, evaluateText)
import Language.Mimsa.Repl.Parser (replParser)
import Language.Mimsa.Repl.Types
import Language.Mimsa.Server.EnvVars
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import System.Console.Haskeline
import System.Directory

-- | Repl uses store in ~/.local/share/mimsa
createMimsaConfig :: MimsaM e MimsaConfig
createMimsaConfig = do
  path <- liftIO $ getXdgDirectory XdgData "mimsa"
  pure $ MimsaConfig 0 path

getProject :: MimsaConfig -> MimsaM (Error ann) (Project ann)
getProject mimsaConfig =
  do
    env <- withExceptT StoreErr loadProject
    let items = length . getStore . prjStore $ env
    logDebug $ "Successfully loaded project, " <> T.pack (show items) <> " store items found"
    pure env
    `catchError` \_ -> do
      logDebug "Failed to load project, loading default project"
      pure defaultProject

repl :: MimsaM (Error Annotation) ()
repl = do
  mimsaConfig <- createMimsaConfig
  env <- getProject mimsaConfig
  runInputT defaultSettings (loop mimsaConfig env)
  where
    loop ::
      MimsaConfig ->
      Project Annotation ->
      InputT (MimsaM (Error Annotation)) ()
    loop mimsaConfig exprs' = do
      minput <- getInputLine ":> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          newEnv <- lift $ parseCommand mimsaConfig exprs' (T.pack input)
          loop mimsaConfig newEnv

parseCommand ::
  MimsaConfig ->
  Project Annotation ->
  Text ->
  MimsaM (Error Annotation) (Project Annotation)
parseCommand mimsaConfig env input =
  case parseAndFormat replParser input of
    Left e -> do
      logError e
      pure env
    Right replAction -> do
      newExprs <- doReplAction mimsaConfig env input replAction
      _ <- runExceptT $ saveProject mimsaConfig newExprs
      pure newExprs
