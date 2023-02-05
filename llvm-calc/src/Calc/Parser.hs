{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser
  ( parseExpr,
    parseExprAndFormatError,
  )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Calc.Parser.Expr
import Calc.Types
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

type ParseErrorType = ParseErrorBundle Text Void

type ParserExpr = Expr Annotation

parseAndFormat :: Parser a -> Text -> Either Text a
parseAndFormat p = first (T.pack . errorBundlePretty) . parse (p <* eof) "repl"

-- parse expr, using it all up
parseExpr :: Text -> Either ParseErrorType ParserExpr
parseExpr = parse (space *> exprParser <* eof) "repl"

parseExprAndFormatError :: Text -> Either Text ParserExpr
parseExprAndFormatError = parseAndFormat (space *> exprParser <* eof)


