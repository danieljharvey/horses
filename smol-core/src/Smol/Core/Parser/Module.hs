{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser.Module
  ( moduleParser,
    DefPart (..),
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

-- we've excluded Export here
parseModuleItem :: Parser [ModuleItem Annotation]
parseModuleItem =
  moduleDefinitionParser
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

annotatedIdentifier :: Parser (Annotated Identifier Annotation)
annotatedIdentifier = withLocation (\a b -> Annotated a b) identifierParser

-- definitions
-- def oneHundred = 100
-- def id a = a
-- def exclaim (str: String) = str ++ "!!!"
-- def exclaim2 (str: String): String = str ++ "!!!"

defPartParser :: Parser (DefPart Annotation)
defPartParser =
  let parseDefArg = DefArg <$> annotatedIdentifier
      parseTypeArg =
        inBrackets
          ( do
              name <- annotatedIdentifier
              myString ":"
              DefTypedArg name <$> typeParser
          )
      parseDefType = do
        myString ":"
        DefType <$> typeParser
   in parseDefType <|> parseTypeArg <|> parseDefArg

-- top level definition
moduleDefinitionParser :: Parser [ModuleItem Annotation]
moduleDefinitionParser = do
  myString "def"
  name <- identifierParser
  parts <-
    chainl1 ((: []) <$> defPartParser) (pure (<>))
      <|> pure mempty
  myString "="
  expr <- expressionParser
  pure [ModuleExpression name parts expr]

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

{-
-- `infix <|> = altMaybe`
parseInfix :: Parser [ModuleItem Annotation]
parseInfix = do
  myString "infix"
  infixOp <- infixOpParser
  myString "="
  boundExpr <- expressionParser
  pure [ModuleInfix infixOp boundExpr]

-- `test "1 + 1 == 2" = 1 + 1 == 2`
parseTest :: Parser [ModuleItem Annotation]
parseTest = do
  myString "test"
  testName <- testNameParser
  myString "="
  boundExpr <- expressionParser
  pure [ModuleTest testName boundExpr]
-}
