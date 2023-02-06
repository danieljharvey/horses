{-# LANGUAGE OverloadedStrings #-}

module Calc.Repl
  ( repl,
  )
where

import Calc.Compile.ToLLVM
import Calc.Parser
import qualified Calc.Compile.RunLLVM as Run
import Control.Monad.IO.Class
import System.Console.Haskeline
import qualified Data.Text as T

repl :: IO ()
repl = do
  putStrLn "Welcome to llvm-calc"
  putStrLn "Exit with :quit"
  runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine ":> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          case parseExprAndFormatError (T.pack input) of
            Left e -> do
              liftIO $ print e
              loop
            Right expr -> do
              resp <- liftIO $ fmap Run.rrResult (Run.run (toLLVM expr))
              liftIO $ putStrLn (T.unpack resp)
              loop
