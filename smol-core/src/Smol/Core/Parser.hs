{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser
  ( parseAndFormat,
    parseExpr,
    parseExprAndFormatError,
    parseTypeAndFormatError,
    parseType,
    parseModule,
    parseConstraint,
    parseModuleAndFormatError,
    parseDataTypeAndFormatError,
    parseConstraintAndFormatError,
    ParseErrorType,
  )
where

import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Smol.Core.Modules.Types
import Smol.Core.Parser.DataType (dataTypeParser)
import Smol.Core.Parser.Expr
import Smol.Core.Parser.Module
import Smol.Core.Parser.Type
import Smol.Core.Parser.Typeclass
import Smol.Core.Typecheck.Types
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

parseModule :: Text -> Either ParseErrorType [ModuleItem Annotation]
parseModule = parse (space *> moduleParser <* eof) "repl"

parseConstraint :: Text -> Either ParseErrorType (Constraint Annotation)
parseConstraint = parse (space *> constraintParser <* eof) "repl"

parseModuleAndFormatError :: Text -> Either Text [ModuleItem Annotation]
parseModuleAndFormatError = parseAndFormat (space *> moduleParser <* eof)

parseDataTypeAndFormatError :: Text -> Either Text (DataType ParseDep Annotation)
parseDataTypeAndFormatError = parseAndFormat (space *> dataTypeParser <* eof)

parseConstraintAndFormatError :: Text -> Either Text (Constraint Annotation)
parseConstraintAndFormatError = parseAndFormat (space *> constraintParser <* eof)
