module Calc.Parser.Types
  ( Parser,
    ParseErrorType,
    ParserExpr,
    ParserType,
    ParserFunction,
  )
where

import Calc.Types.Annotation
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Type
import Data.Text (Text)
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text

type ParseErrorType = ParseErrorBundle Text Void

type ParserExpr = Expr Annotation

type ParserType = Type Annotation

type ParserFunction = Function Annotation
