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
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import System.Console.Haskeline

repl :: IO ()
repl = do
  loadedEnv <- runExceptT loadProject
  env <- case loadedEnv of
    Right env' -> do
      let items = length . getStore . store $ env'
      T.putStrLn $ "Successfully loaded project, " <> T.pack (show items) <> " store items found"
      pure env'
    _ -> do
      T.putStrLn "Failed to load project, loading default project"
      pure defaultProject
  _ <- doReplAction env "" Help
  runInputT defaultSettings (loop env)
  where
    loop ::
      Project Annotation ->
      InputT IO ()
    loop exprs' = do
      minput <- getInputLine ":> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          newEnv <- liftIO $ parseCommand exprs' (T.pack input)
          loop newEnv

parseCommand ::
  Project Annotation ->
  Text ->
  IO (Project Annotation)
parseCommand env input =
  case parseAndFormat replParser input of
    Left e -> do
      T.putStrLn e
      pure env
    Right replAction -> do
      newExprs <- doReplAction env input replAction
      _ <- runExceptT $ saveProject newExprs
      pure newExprs
