{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Watcher where

import Data.List (isInfixOf)
import System.FSNotify
import System.IO

doWeCareAboutThisEvent :: Event -> Bool
doWeCareAboutThisEvent (Modified path _ _) = "scratch.mimsa" `isInfixOf` path
doWeCareAboutThisEvent _ = False

waitForKeyPress :: IO ()
waitForKeyPress = do
  bufferMode <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  _ <- getChar
  hSetBuffering stdin bufferMode
  pure ()

watchFile :: String -> IO () -> IO ()
watchFile path action =
  withManager $ \mgr -> do
    _ <-
      watchDir
        mgr -- manager
        path -- directory to watch
        doWeCareAboutThisEvent -- predicate
        (const action) -- action
    waitForKeyPress
