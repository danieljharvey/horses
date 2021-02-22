module Language.Mimsa.CLI (cli) where

import Control.Applicative
import Language.Mimsa.Repl (repl)
import Language.Mimsa.Server (server)
import qualified Options.Applicative as Opt

data CLICommand
  = Repl Bool
  | Server

cli :: IO ()
cli = do
  command' <-
    Opt.execParser (Opt.info (command <**> Opt.helper) Opt.fullDesc)
      <|> pure (Repl False)
  case command' of
    Repl showLogs -> repl showLogs
    Server -> server

showLogsParser :: Opt.Parser Bool
showLogsParser =
  Opt.flag' True (Opt.short 'v' <> Opt.long "verbose")
    <|> pure False

command :: Opt.Parser CLICommand
command =
  Opt.hsubparser
    ( Opt.command
        "repl"
        ( Opt.info
            (Repl <$> showLogsParser)
            (Opt.progDesc "Start the Mimsa repl")
        )
        <> Opt.command
          "server"
          ( Opt.info
              (pure Server)
              (Opt.progDesc "Start the Mimsa server")
          )
    )
