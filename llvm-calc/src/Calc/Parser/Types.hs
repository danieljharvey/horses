module Calc.Parser.Types
  ( Parser, ParseErrorType, ParserExpr
  )
where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Calc.Types.Expr
import Calc.Types.Annotation

type Parser = Parsec Void Text

type ParseErrorType = ParseErrorBundle Text Void

type ParserExpr = Expr Annotation

