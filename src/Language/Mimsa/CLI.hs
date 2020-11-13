module Language.Mimsa.CLI (cli) where

import Control.Applicative
import Language.Mimsa.Repl (repl)
import Language.Mimsa.Server.Type (server)
import qualified Options.Applicative as Opt

data CLICommand = Repl | Server

cli :: IO ()
cli = do
  command' <- Opt.execParser (Opt.info (command <**> Opt.helper) Opt.fullDesc) <|> pure Repl
  case command' of
    Repl -> repl
    Server -> server

command :: Opt.Parser CLICommand
command =
  Opt.hsubparser
    ( Opt.command
        "repl"
        ( Opt.info
            (pure Repl)
            (Opt.progDesc "Start the Mimsa repl")
        )
        <> Opt.command
          "server"
          ( Opt.info
              (pure Server)
              (Opt.progDesc "Start the Mimsa server")
          )
    )
