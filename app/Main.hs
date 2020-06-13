{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Mimsa

main :: IO ()
main = do
  putStrLn "--- IT'S BUSINESS TIME ---"
  repl
