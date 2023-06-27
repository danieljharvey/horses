module Repl where

import Control.Applicative
import Data.Text (Text)
import qualified Options.Applicative as Opt
import qualified Smol.Check as Check
import qualified Smol.Repl as Repl
import System.IO

data AppAction
  = Repl
  | Check Text -- check if a file is `ok`

parseAppAction :: Opt.Parser AppAction
parseAppAction =
  Opt.hsubparser
    ( Opt.command
        "repl"
        ( Opt.info
            (pure Repl)
            (Opt.progDesc "Start new module-based Smol repl")
        )
        <> Opt.command
          "check"
          ( Opt.info
              (Check <$> filePathParse)
              (Opt.progDesc "Check whether a file is valid and OK etc")
          )
    )

filePathParse :: Opt.Parser Text
filePathParse =
  Opt.argument
    Opt.str
    (Opt.metavar "<file path>")

optionsParse :: Opt.Parser AppAction
optionsParse = parseAppAction

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
  action <-
    Opt.customExecParser
      helpfulPreferences
      (Opt.info (optionsParse <**> Opt.helper) Opt.fullDesc)
  case action of
    Repl -> Repl.repl
    Check filePath -> Check.check filePath
