module Main where

import Control.Applicative
import qualified Options.Applicative as Opt
import Server.Main (server)
import Server.Swagger (outputJSON)
import System.IO

data CLICommand
  = GenerateSwagger
  | Server

cli :: IO ()
cli = do
  command' <-
    Opt.execParser (Opt.info (command <**> Opt.helper) Opt.fullDesc)
      <|> pure Server
  case command' of
    GenerateSwagger -> outputJSON
    Server -> server

showLogsParser :: Opt.Parser Bool
showLogsParser =
  Opt.flag' True (Opt.short 'v' <> Opt.long "verbose")
    <|> pure False

command :: Opt.Parser CLICommand
command =
  Opt.hsubparser
    ( Opt.command
        "generate-swagger"
        ( Opt.info
            (pure GenerateSwagger)
            (Opt.progDesc "Generate swagger.json for server")
        )
    )
    <|> pure Server

-- | this runs the server
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  cli
