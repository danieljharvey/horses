{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser
  ( parseExpr,
    parseExprAndFormatError,
    parseType,
    parseTypeAndFormatError,
    parseFunction,
    parseFunctionAndFormatError,
    parseModule,
    parseModuleAndFormatError,
    replFilename,
  )
where

import Calc.Parser.Expr
import Calc.Parser.Function
import Calc.Parser.Module
import Calc.Parser.Type
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

-- parse type, using it all up
parseType :: Text -> Either ParseErrorType ParserType
parseType = parse (space *> typeParser <* eof) replFilename

-- | `parseType`, but format error to text
parseTypeAndFormatError :: Text -> Either Text ParserType
parseTypeAndFormatError = parseAndFormat (space *> typeParser <* eof)

-- parse function, using it all up
parseFunction :: Text -> Either ParseErrorType ParserFunction
parseFunction = parse (space *> functionParser <* eof) replFilename

-- | `parseType`, but format error to text
parseFunctionAndFormatError :: Text -> Either Text ParserFunction
parseFunctionAndFormatError = parseAndFormat (space *> functionParser <* eof)

-- parse module, using it all up
parseModule :: Text -> Either ParseErrorType ParserModule
parseModule = parse (space *> moduleParser <* eof) replFilename

-- | `parseModule`, but format error to text
parseModuleAndFormatError :: Text -> Either Text ParserModule
parseModuleAndFormatError = parseAndFormat (space *> moduleParser <* eof)
