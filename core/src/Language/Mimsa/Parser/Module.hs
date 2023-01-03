{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Module
  ( parseModule,
    moduleParser,
    DefPart (..),
  )
where

import Data.Char as Char
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifier
import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Parser.Language
import Language.Mimsa.Parser.Lexeme
import Language.Mimsa.Parser.Literal
import Language.Mimsa.Parser.MonoType
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Module.Module
import Language.Mimsa.Types.Module.ModuleHash
import Text.Megaparsec hiding (parseTest)
import Text.Megaparsec.Char

parseModule :: Text -> Either ParseErrorType [ModuleItem Annotation]
parseModule = parse (space *> moduleParser <* eof) "repl"

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
  parseDef
    <|> try typeParser
    <|> parseImport
    <|> parseInfix
    <|> parseTest

-------

-- type definitions
-- type Maybe a = Just a | Nothing
-- type Tree a = Branch (Tree a) a (Tree a) | Leaf a
typeParser :: Parser [ModuleItem Annotation]
typeParser = do
  td <- typeDeclParser
  pure [ModuleDataType td]

-------

-- definitions
-- def oneHundred = 100
-- def id a = a
-- def exclaim (str: String) = str ++ "!!!"
-- def exclaim2 (str: String): String = str ++ "!!!"

defPartParser :: Parser (DefPart Annotation)
defPartParser =
  let parseDefArg = DefArg <$> identifierParser
      parseTypeArg =
        inBrackets
          ( do
              name <- identifierParser
              myString ":"
              DefTypedArg name <$> monoTypeParser
          )
      parseDefType = do
        myString ":"
        DefType <$> monoTypeParser
   in parseDefType <|> parseTypeArg <|> parseDefArg

-- top level definition
parseDef :: Parser [ModuleItem Annotation]
parseDef = do
  myString "def"
  name <- nameParser
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
