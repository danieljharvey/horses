{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.TypeDecl
  ( typeDeclParser,
    parseTypeDeclAndFormatError,
  )
where

import qualified Control.Monad.Combinators.Expr as PC
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers (TyCon)
import Text.Megaparsec
import Text.Megaparsec.Char

parseTypeDeclAndFormatError :: Text -> Either Text DataType
parseTypeDeclAndFormatError =
  parseAndFormat (typeDeclParser <* eof)

typeDeclParser :: Parser DataType
typeDeclParser =
  try typeDeclParserWithCons
    <|> try typeDeclParserEmpty

-- it's your "type Void in ..."
typeDeclParserEmpty :: Parser DataType
typeDeclParserEmpty = do
  _ <- thenSpace (string "type")
  tyName <- tyConParser
  pure (DataType tyName mempty mempty)

-- it's your more complex cases
typeDeclParserWithCons :: Parser DataType
typeDeclParserWithCons = do
  _ <- thenSpace (string "type")
  tyName <- thenSpace tyConParser
  tyArgs <- try $ many (thenSpace nameParser)
  _ <- thenSpace (string "=")
  constructors <-
    try manyTypeConstructors
      <|> try oneTypeConstructor
  pure $ DataType tyName tyArgs constructors

--------

manyTypeConstructors :: Parser (Map TyCon [Field])
manyTypeConstructors = do
  tyCons <-
    sepBy
      (withOptionalSpace oneTypeConstructor)
      (literalWithSpace "|")
  pure (mconcat tyCons)

-----

oneTypeConstructor :: Parser (Map TyCon [Field])
oneTypeConstructor = do
  name <- tyConParser
  args <-
    try
      ( do
          _ <- space1
          sepBy (withOptionalSpace typeNameParser) space
      )
      <|> pure mempty
  pure (M.singleton name args)

-----

typeNameParser :: Parser Field
typeNameParser =
  try emptyConsParser
    <|> try (inBrackets functionParser)
    <|> try varNameParser
    <|> try (inBrackets parameterisedConsParser)

-- Simple type like String
emptyConsParser :: Parser Field
emptyConsParser = ConsName <$> tyConParser <*> pure mempty

--
parameterisedConsParser :: Parser Field
parameterisedConsParser = do
  c <- tyConParser
  let itemParser = do
        _ <- space1
        typeNameParser
  params <- some itemParser
  pure $ ConsName c params

varNameParser :: Parser Field
varNameParser = VarName <$> nameParser

--------

arrParse :: PC.Operator Parser Field
arrParse = PC.InfixR $ do
  _ <- space1
  _ <- thenSpace (string "->")
  pure TNFunc

functionParser :: Parser Field
functionParser =
  PC.makeExprParser typeNameParser [[arrParse]]
