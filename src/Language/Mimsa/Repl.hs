{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl
  ( repl,
    evaluateText,
  )
where

import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Monad
import Language.Mimsa.Parser
import Language.Mimsa.Project
  ( defaultProject,
    loadProject,
    saveProject,
  )
import Language.Mimsa.Repl.Actions (doReplAction, evaluateText)
import Language.Mimsa.Repl.Parser (replParser)
import Language.Mimsa.Server.EnvVars
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import System.Console.Haskeline
import System.Directory

-- | Repl uses store in ~/.local/share/mimsa
createMimsaConfig :: IO MimsaConfig
createMimsaConfig = do
  path <- getXdgDirectory XdgData "mimsa"
  pure $ MimsaConfig 0 path

getProject :: (Monoid ann) => MimsaM (Error ann) (Project ann)
getProject =
  do
    env <- mapError StoreErr loadProject
    let items = length . getStore . prjStore $ env
    logInfo $ "Successfully loaded project, " <> T.pack (show items) <> " store items found"
    pure env
    `catchError` \_ -> do
      logInfo "Failed to load project, loading default project"
      pure defaultProject

repl :: IO ()
repl = do
  mimsaConfig <- createMimsaConfig
  _ <- runMimsaM mimsaConfig replM
  pure ()

replM :: MimsaM (Error Annotation) ()
replM = do
  env <- getProject
  runInputT defaultSettings (loop env)
  where
    loop ::
      Project Annotation ->
      InputT (MimsaM (Error Annotation)) ()
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
  MimsaM (Error Annotation) (Project Annotation)
parseCommand env input =
  case parseAndFormat replParser input of
    Left e -> do
      logError e
      pure env
    Right replAction -> do
      newExprs <- doReplAction env input replAction
      _ <- mapError StoreErr (saveProject newExprs)
      pure newExprs
