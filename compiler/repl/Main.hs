module Main where

import Control.Applicative
import qualified Options.Applicative as Opt
import Repl.Repl (repl)
import System.IO

-- | this runs the repl
showLogsParser :: Opt.Parser Bool
showLogsParser =
  Opt.flag' True (Opt.short 'v' <> Opt.long "verbose")
    <|> pure False

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  showLogs <-
    Opt.execParser (Opt.info (showLogsParser <**> Opt.helper) Opt.fullDesc)
  repl showLogs
