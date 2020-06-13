{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Mimsa.Parser as P
import Language.Mimsa.Repl.Actions (doReplAction)
import Language.Mimsa.Repl.Parser (replParser)
import Language.Mimsa.Store
  ( Environment (..),
    loadEnvironment,
    saveEnvironment,
  )
import System.Console.Haskeline

repl :: IO ()
repl = do
  env <- loadEnvironment
  runInputT defaultSettings (loop (fromMaybe mempty env))
  where
    loop :: Environment -> InputT IO ()
    loop exprs' = do
      minput <- getInputLine ":> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          newEnv <- liftIO $ parseCommand exprs' (T.pack input)
          loop newEnv

parseCommand :: Environment -> Text -> IO Environment
parseCommand env input = case P.runParserComplete replParser input of
  Left e -> do
    T.putStrLn e
    pure env
  Right replAction -> do
    newExprs <- doReplAction env replAction
    saveEnvironment newExprs
    pure newExprs
