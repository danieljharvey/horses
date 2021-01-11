{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.TypeDecl
  ( typeDeclParser,
  )
where

import qualified Control.Monad.Combinators.Expr as PC
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers (TyCon, TypeName (..))
import Text.Megaparsec
import Text.Megaparsec.Char

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

manyTypeConstructors :: Parser (Map TyCon [TypeName])
manyTypeConstructors = do
  tyCons <-
    sepBy
      (withOptionalSpace oneTypeConstructor)
      (literalWithSpace "|")
  pure (mconcat tyCons)

-----

oneTypeConstructor :: Parser (Map TyCon [TypeName])
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

typeNameParser :: Parser TypeName
typeNameParser =
  try emptyConsParser
    <|> try (inBrackets functionParser)
    <|> try varNameParser
    <|> try (inBrackets parameterisedConsParser)

-- Simple type like String
emptyConsParser :: Parser TypeName
emptyConsParser = ConsName <$> tyConParser <*> pure mempty

--
parameterisedConsParser :: Parser TypeName
parameterisedConsParser = do
  c <- thenSpace tyConParser
  params <- try (sepBy (withOptionalSpace typeNameParser) space1) <|> pure mempty
  pure $ ConsName c params

varNameParser :: Parser TypeName
varNameParser = VarName <$> nameParser

--------

arrParse :: PC.Operator Parser TypeName
arrParse = PC.InfixR $ do
  _ <- space1
  _ <- thenSpace (string "->")
  pure TNFunc

functionParser :: Parser TypeName
functionParser =
  PC.makeExprParser typeNameParser [[arrParse]]
