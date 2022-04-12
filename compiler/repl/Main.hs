module Main where

import Control.Applicative
import qualified Init.Main as Init
import qualified Options.Applicative as Opt
import qualified Repl.Main as Repl
import System.IO

-- | this runs the repl
parseShowLogs :: Opt.Parser Bool
parseShowLogs =
  Opt.flag' True (Opt.short 'v' <> Opt.long "verbose")
    <|> pure False

data AppAction
  = Repl
  | Init

parseAppAction :: Opt.Parser AppAction
parseAppAction =
  Opt.hsubparser
    ( Opt.command
        "repl"
        ( Opt.info
            (pure Repl)
            (Opt.progDesc "Start a Mimsa repl")
        )
        <> Opt.command
          "init"
          ( Opt.info
              (pure Init)
              (Opt.progDesc "Create a new mimsa project in the current folder")
          )
    )

optionsParse :: Opt.Parser (AppAction, Bool)
optionsParse = (,) <$> parseAppAction <*> parseShowLogs

helpfulPreferences :: Opt.ParserPrefs
helpfulPreferences =
  Opt.defaultPrefs
    { Opt.prefShowHelpOnError = True,
      Opt.prefShowHelpOnEmpty = True
    }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  (action, showLogs) <-
    Opt.customExecParser
      helpfulPreferences
      (Opt.info (optionsParse <**> Opt.helper) Opt.fullDesc)
  case action of
    Init -> Init.init showLogs
    Repl -> Repl.repl showLogs
