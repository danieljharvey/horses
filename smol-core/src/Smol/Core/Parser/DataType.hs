{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser.DataType (dataTypeParser) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Void (Void)
import Smol.Core.Parser.Identifiers
import Smol.Core.Parser.Shared
import Smol.Core.Parser.Type
import Smol.Core.Types
import Text.Megaparsec

type Parser = Parsec Void Text

dataTypeParser :: Parser (DataType ParseDep Annotation)
dataTypeParser =
  try typeDeclParserWithCons
    <|> typeDeclParserEmpty

-- it's your "type Void in ..."
typeDeclParserEmpty :: Parser (DataType ParseDep Annotation)
typeDeclParserEmpty = do
  myString "type"
  tyName <- plainTypeNameParser
  pure (DataType tyName mempty mempty)

-- it's your more complex cases
typeDeclParserWithCons :: Parser (DataType ParseDep Annotation)
typeDeclParserWithCons = do
  myString "type"
  tyName <- plainTypeNameParser
  tyArgs <- many identifierParser
  myString "="
  DataType tyName tyArgs <$> manyTypeConstructors

--------

manyTypeConstructors :: Parser (Map Constructor [ParsedType Annotation])
manyTypeConstructors = do
  tyCons <-
    sepBy
      oneTypeConstructor
      (myString "|")
  pure (mconcat tyCons)

-----

oneTypeConstructor :: Parser (Map Constructor [ParsedType Annotation])
oneTypeConstructor = do
  constructor <- myLexeme constructorParserInternal
  args <-
    some (try simpleTypeParser <|> inBrackets typeParser)
      <|> pure mempty
  pure (M.singleton constructor args)
