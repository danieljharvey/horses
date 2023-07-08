{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser.Module
  ( moduleParser,
  )
where

import Data.Text (Text)
import Data.Void
import Smol.Core.Parser.DataType (dataTypeParser)
import Smol.Core.Parser.Expr
import Smol.Core.Parser.Identifiers
import Smol.Core.Parser.Shared
import Smol.Core.Parser.Type
import Smol.Core.Types
import Smol.Core.Modules.Types.ModuleItem
import Text.Megaparsec hiding (parseTest)

type Parser = Parsec Void Text

-- currently fails at the first hurdle
-- since we can parse each thing separately, maybe
-- we should be making each throw errors for later, but returning `mempty` so
-- we can collect all of the separate parse errors at once?
-- use `registerParseError` from https://hackage.haskell.org/package/megaparsec-9.2.1/docs/Text-Megaparsec.html
moduleParser :: Parser [ModuleItem Annotation]
moduleParser =
  mconcat
    <$> ( chainl1 ((\a -> [[a]]) <$> parseModuleItem) (pure (<>))
            <|> pure mempty
        )

-- we've excluded Export here
parseModuleItem :: Parser (ModuleItem Annotation)
parseModuleItem =
  try moduleTypeDefinitionParser
    <|> try moduleDefinitionParser
    <|> try moduleTypeDeclarationParser
    <|> parseTest

--    <|> parseInfix

-------

-- type definitions
-- type Maybe a = Just a | Nothing
-- type Tree a = Branch (Tree a) a (Tree a) | Leaf a
moduleTypeDeclarationParser :: Parser (ModuleItem Annotation)
moduleTypeDeclarationParser = ModuleDataType <$> dataTypeParser

-------

-- definitions
-- def oneHundred = 100
-- def id a = a
-- def const a b = a
--
-- top level definition
moduleDefinitionParser :: Parser (ModuleItem Annotation)
moduleDefinitionParser = do
  myString "def"
  name <- identifierParser
  parts <-
    chainl1 ((: []) <$> identifierParser) (pure (<>))
      <|> pure mempty
  myString "="
  ModuleExpression name parts <$> expressionParser

-- top level type definition
-- def id : a -> a
-- def compose : (b -> c) -> (a -> b) -> (a -> c)
moduleTypeDefinitionParser :: Parser (ModuleItem Annotation)
moduleTypeDefinitionParser = do
  myString "def"
  name <- identifierParser
  myString ":"
  ModuleExpressionType name <$> typeParser

-- `test "everything is fine" with myFunctionName`
parseTest :: Parser (ModuleItem Annotation)
parseTest = do
  myString "test"
  testName <- testNameParser
  myString "using"
  ModuleTest testName <$> identifierParser
