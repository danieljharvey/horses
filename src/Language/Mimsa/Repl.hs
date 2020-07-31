{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl
  ( repl,
    evaluateText,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Repl.Actions (doReplAction, evaluateText)
import Language.Mimsa.Repl.Parser (replParser)
import Language.Mimsa.Repl.Types
import Language.Mimsa.Store (loadEnvironment, saveEnvironment)
import qualified Language.Mimsa.Syntax as P
import Language.Mimsa.Types
import System.Console.Haskeline

repl :: IO ()
repl = do
  env <- fromMaybe mempty <$> loadEnvironment
  _ <- doReplAction env Help
  runInputT defaultSettings (loop env)
  where
    loop :: Project -> InputT IO ()
    loop exprs' = do
      minput <- getInputLine ":> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          newEnv <- liftIO $ parseCommand exprs' (T.pack input)
          loop newEnv

parseCommand :: Project -> Text -> IO Project
parseCommand env input = case P.runParserComplete replParser input of
  Left e -> do
    T.putStrLn e
    pure env
  Right replAction -> do
    newExprs <- doReplAction env replAction
    saveEnvironment newExprs
    pure newExprs
