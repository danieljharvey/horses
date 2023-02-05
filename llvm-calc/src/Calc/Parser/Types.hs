module Calc.Parser.Types
  ( Parser,
    ParseErrorType,
    ParserExpr,
  )
where

import Calc.Types.Annotation
import Calc.Types.Expr
import Data.Text (Text)
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text

type ParseErrorType = ParseErrorBundle Text Void

type ParserExpr = Expr Annotation
