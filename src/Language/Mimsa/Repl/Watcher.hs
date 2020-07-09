{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Watcher where

-- for FilePath literals

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.List (isInfixOf)
import System.FSNotify

doWeCareAboutThisEvent :: Event -> Bool
doWeCareAboutThisEvent (Modified path _ _) = "scratch.mimsa" `isInfixOf` path
doWeCareAboutThisEvent _ = False

watchFile :: String -> IO () -> IO ()
watchFile path action =
  withManager $ \mgr -> do
    -- start a watching job (in the background)
    _ <-
      watchDir
        mgr -- manager
        path -- directory to watch
        doWeCareAboutThisEvent -- predicate
        (const action) -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
