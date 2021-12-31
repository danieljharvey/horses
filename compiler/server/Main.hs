module Main where

import Language.Mimsa.Server (server)
import System.IO

-- | this runs the server
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  server
