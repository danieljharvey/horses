module Router.Main (main) where

import Router.Config
import Router.Server

main :: IO ()
main = do
  cfg <- readConfig
  runServer cfg
