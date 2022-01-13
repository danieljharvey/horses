{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.TypeDecl
  ( typeDeclParser,
    parseTypeDeclAndFormatError,
  )
where

import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Parser.MonoType (monoTypeParser)
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers (TyCon)
import Language.Mimsa.Types.NullUnit
import Language.Mimsa.Types.Typechecker
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

manyTypeConstructors :: Parser (Map TyCon [Type NullUnit])
manyTypeConstructors = do
  tyCons <-
    sepBy
      (withOptionalSpace oneTypeConstructor)
      (literalWithSpace "|")
  pure (mconcat tyCons)

-----

oneTypeConstructor :: Parser (Map TyCon [Type NullUnit])
oneTypeConstructor = do
  name <- tyConParser
  args <-
    try
      ( do
          _ <- space1
          sepBy (withOptionalSpace monoTypeParser) space
      )
      <|> pure mempty
  let argsWithNoType = ($> NullUnit) <$> args
  pure (M.singleton name argsWithNoType)

-----
