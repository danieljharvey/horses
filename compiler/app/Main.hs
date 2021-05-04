module Main where

import Language.Mimsa
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  cli
