{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser
  ( parseAndFormat,
    parseExpr,
    parseExprAndFormatError,
    parseTypeAndFormatError,
    parseType,
    parseConstraint,
    parseDataTypeAndFormatError,
    parseConstraintAndFormatError,
    ParseErrorType,
  )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Smol.Core.Parser.DataType (dataTypeParser)
import Smol.Core.Parser.Expr
import Smol.Core.Parser.Type
import Smol.Core.Parser.Typeclass
import Smol.Core.Types
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

type ParseErrorType = ParseErrorBundle Text Void

type ParserExpr = Expr ParseDep Annotation

parseAndFormat :: Parser a -> Text -> Either Text a
parseAndFormat p = first (T.pack . errorBundlePretty) . parse (p <* eof) "repl"

-- parse expr, using it all up
parseExpr :: Text -> Either ParseErrorType ParserExpr
parseExpr = parse (space *> expressionParser <* eof) "repl"

parseExprAndFormatError :: Text -> Either Text ParserExpr
parseExprAndFormatError = parseAndFormat (space *> expressionParser <* eof)

parseConstraint :: Text -> Either ParseErrorType (Constraint ParseDep Annotation)
parseConstraint = parse (space *> constraintParser <* eof) "repl"

parseDataTypeAndFormatError :: Text -> Either Text (DataType ParseDep Annotation)
parseDataTypeAndFormatError = parseAndFormat (space *> dataTypeParser <* eof)

parseConstraintAndFormatError :: Text -> Either Text (Constraint ParseDep Annotation)
parseConstraintAndFormatError = parseAndFormat (space *> constraintParser <* eof)
