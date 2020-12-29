{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl
  ( repl,
    evaluateText,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import System.Console.Haskeline
import System.Directory

-- | Repl uses store in ~/.local/share/mimsa
createMimsaConfig :: IO MimsaConfig
createMimsaConfig = do
  path <- getXdgDirectory XdgData "mimsa"
  pure $ MimsaConfig 0 path

repl :: IO ()
repl = do
  mimsaConfig <- createMimsaConfig
  loadedEnv <- runExceptT (loadProject mimsaConfig)
  env <- case loadedEnv of
    Right env' -> do
      let items = length . getStore . store $ env'
      T.putStrLn $ "Successfully loaded project, " <> T.pack (show items) <> " store items found"
      pure env'
    _ -> do
      T.putStrLn "Failed to load project, loading default project"
      pure defaultProject
  _ <- doReplAction mimsaConfig env "" Help
  runInputT defaultSettings (loop mimsaConfig env)
  where
    loop ::
      MimsaConfig ->
      Project Annotation ->
      InputT IO ()
    loop mimsaConfig exprs' = do
      minput <- getInputLine ":> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          newEnv <- liftIO $ parseCommand mimsaConfig exprs' (T.pack input)
          loop mimsaConfig newEnv

parseCommand ::
  MimsaConfig ->
  Project Annotation ->
  Text ->
  IO (Project Annotation)
parseCommand mimsaConfig env input =
  case parseAndFormat replParser input of
    Left e -> do
      T.putStrLn e
      pure env
    Right replAction -> do
      newExprs <- doReplAction mimsaConfig env input replAction
      _ <- runExceptT $ saveProject mimsaConfig newExprs
      pure newExprs
