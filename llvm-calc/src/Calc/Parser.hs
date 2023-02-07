{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser
  ( parseExpr,
    parseExprAndFormatError,
    replFilename,
  )
where

import Calc.Parser.Expr
import Calc.Parser.Types
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

-- | which file are we parsing?
-- we use this to show the right text in errors
replFilename :: FilePath
replFilename = "repl"

parseAndFormat :: Parser a -> Text -> Either Text a
parseAndFormat p =
  first (T.pack . errorBundlePretty)
    . parse (p <* eof) "repl"

-- parse expr, using it all up
parseExpr :: Text -> Either ParseErrorType ParserExpr
parseExpr = parse (space *> exprParser <* eof) "repl"

-- | `parseExpr`, but format error to text
parseExprAndFormatError :: Text -> Either Text ParserExpr
parseExprAndFormatError = parseAndFormat (space *> exprParser <* eof)
