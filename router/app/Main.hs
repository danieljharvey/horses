module Main where

import qualified Router.Main as Router
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "Starting Router..."
  Router.main
