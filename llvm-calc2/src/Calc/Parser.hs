{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser
  ( parseExpr,
    parseExprAndFormatError,
    parseType,
    parseTypeAndFormatError,
    replFilename,
  )
where

import Calc.Parser.Type
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
    . parse (p <* eof) replFilename

-- parse expr, using it all up
parseExpr :: Text -> Either ParseErrorType ParserExpr
parseExpr = parse (space *> exprParser <* eof) replFilename

-- | `parseExpr`, but format error to text
parseExprAndFormatError :: Text -> Either Text ParserExpr
parseExprAndFormatError = parseAndFormat (space *> exprParser <* eof)

-- parse expr, using it all up
parseType :: Text -> Either ParseErrorType ParserType
parseType = parse (space *> typeParser <* eof) replFilename

-- | `parseType`, but format error to text
parseTypeAndFormatError :: Text -> Either Text ParserType
parseTypeAndFormatError = parseAndFormat (space *> typeParser <* eof)
