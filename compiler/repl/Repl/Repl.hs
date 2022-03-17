{-# LANGUAGE OverloadedStrings #-}

module Repl.Repl
  ( repl,
  )
where

import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Monad
import Language.Mimsa.Parser
import Language.Mimsa.Project
  ( loadProject,
    saveProject,
  )
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.MimsaConfig
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Repl.Actions (doReplAction)
import Repl.Parser (replParser)
import System.Console.Haskeline
import System.Directory

-- | Repl uses store in ~/.local/share/mimsa
-- TODO: change this to local folder
createMimsaConfig :: Bool -> IO MimsaConfig
createMimsaConfig showLogs' = do
  path <- getXdgDirectory XdgData "mimsa"
  pure $ MimsaConfig 0 path showLogs' Nothing

getProject :: MimsaM (Error Annotation) (Project Annotation)
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
  mimsaConfig <- createMimsaConfig showLogs'
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
      replOutput e
      pure env
    Right replAction -> do
      newExprs <- doReplAction env input replAction
      _ <- mapError StoreErr (saveProject newExprs)
      pure newExprs
