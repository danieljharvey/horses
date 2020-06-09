{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

main :: IO ()
main = do
  putStrLn "--- IT'S BUSINESS TIME ---"
  repl
