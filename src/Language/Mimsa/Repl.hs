{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl
  ( repl,
    evaluateText,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Functor (($>))
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
import Language.Mimsa.Types
import System.Console.Haskeline
import Text.Megaparsec

repl :: IO ()
repl = do
  loadedEnv <- runExceptT loadProject
  let env = case loadedEnv $> mempty of
        Right env' -> env'
        _ -> defaultProject
  _ <- doReplAction env Help
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
  case parseAndFormat (replParser <* eof) input of
    Left e -> do
      T.putStrLn e
      pure env
    Right replAction -> do
      newExprs <- doReplAction env replAction
      _ <- runExceptT $ saveProject newExprs
      pure newExprs
