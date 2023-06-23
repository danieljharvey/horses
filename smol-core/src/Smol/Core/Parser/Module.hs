{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser.Module
  ( moduleParser,
  )
where

import Data.Char as Char
import Data.Text (Text)
import Data.Void
import Smol.Core.Parser.DataType (dataTypeParser)
import Smol.Core.Parser.Expr
import Smol.Core.Parser.Identifiers
import Smol.Core.Parser.Shared
import Smol.Core.Parser.Type
import Smol.Core.Types
import Smol.Core.Types.Module.Module
import Smol.Core.Types.Module.ModuleHash
import Text.Megaparsec hiding (parseTest)

type Parser = Parsec Void Text

-- currently fails at the first hurdle
-- since we can parse each thing separately, maybe
-- we should be making each throw errors for later, but returning `mempty` so
-- we can collect all of the separate parse errors at once?
-- use `registerParseError` from https://hackage.haskell.org/package/megaparsec-9.2.1/docs/Text-Megaparsec.html
moduleParser :: Parser [ModuleItem Annotation]
moduleParser =
  let bigParsers = parseModuleItem <|> parseExport
   in mconcat
        <$> ( chainl1 ((: []) <$> bigParsers) (pure (<>))
                <|> pure mempty
            )

-- why is this fucked? because we don't stop parsing at the end of a def
-- parsing >>>
-- id : a -> a
-- id a = a
-- <<<
-- fails because we continue parsing the definition as part of the type
-- therefore we need indentation sensitive parsers:
-- https://markkarpov.com/tutorial/megaparsec.html#indentationsensitive-parsing
--
-- we should try parsing as "the identifier should not be indented but the rest
-- must be"

-- we've excluded Export here
parseModuleItem :: Parser [ModuleItem Annotation]
parseModuleItem =
  try moduleTypeDefinitionParser
    <|> try moduleDefinitionParser
    <|> try moduleTypeDeclarationParser
    <|> parseImport

--    <|> parseInfix
--    <|> parseTest

-------

-- type definitions
-- type Maybe a = Just a | Nothing
-- type Tree a = Branch (Tree a) a (Tree a) | Leaf a
moduleTypeDeclarationParser :: Parser [ModuleItem Annotation]
moduleTypeDeclarationParser = do
  td <- dataTypeParser
  pure [ModuleDataType td]

-------

-- definitions
-- oneHundred = 100
-- id a = a
-- const a b = a
--
-- top level definition
moduleDefinitionParser :: Parser [ModuleItem Annotation]
moduleDefinitionParser = do
  name <- identifierParser
  parts <-
    chainl1 ((: []) <$> identifierParser) (pure (<>))
      <|> pure mempty
  myString "="
  expr <- expressionParser
  pure [ModuleExpression name parts expr]

-- top level type definition
-- id : a -> a
-- compose : (b -> c) -> (a -> b) -> (a -> c)
moduleTypeDefinitionParser :: Parser [ModuleItem Annotation]
moduleTypeDefinitionParser = do
  name <- identifierParser
  myString ":"
  ty <- typeParser
  pure [ModuleExpressionType name ty]

parseExport :: Parser [ModuleItem Annotation]
parseExport = do
  myString "export"
  items <- parseModuleItem
  pure (ModuleExport <$> items)

parseHash :: Parser ModuleHash
parseHash =
  ModuleHash
    <$> myLexeme
      ( takeWhile1P (Just "module hash") Char.isAlphaNum
      )

-- TODO: maybe make these into one parser that handles both to avoid
-- backtracking
parseImport :: Parser [ModuleItem Annotation]
parseImport = try parseImportAll <|> parseImportNamed

-- `import Prelude from a123123bcbcbcb`
parseImportNamed :: Parser [ModuleItem Annotation]
parseImportNamed = do
  myString "import"
  modName <- moduleNameParser
  myString "from"
  hash <- parseHash
  pure [ModuleImport (ImportNamedFromHash hash modName)]

-- `import * from a123123bcbcbcb`
parseImportAll :: Parser [ModuleItem Annotation]
parseImportAll = do
  myString "import"
  myString "*"
  myString "from"
  hash <- parseHash
  pure [ModuleImport (ImportAllFromHash hash)]
