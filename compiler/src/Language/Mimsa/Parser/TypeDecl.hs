{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.TypeDecl
  ( typeDeclParser,
    parseTypeDecl,
  )
where

import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Parser.Lexeme
import Language.Mimsa.Parser.MonoType
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers (TyCon)
import Language.Mimsa.Types.NullUnit
import Language.Mimsa.Types.Typechecker
import Text.Megaparsec

parseTypeDecl :: Text -> Either ParseErrorType DataType
parseTypeDecl =
  parse (typeDeclParser <* eof) "type"

typeDeclParser :: Parser DataType
typeDeclParser =
  try typeDeclParserWithCons
    <|> typeDeclParserEmpty

-- it's your "type Void in ..."
typeDeclParserEmpty :: Parser DataType
typeDeclParserEmpty = do
  myString "type"
  tyName <- tyConParser
  pure (DataType tyName mempty mempty)

-- it's your more complex cases
typeDeclParserWithCons :: Parser DataType
typeDeclParserWithCons = do
  myString "type"
  tyName <- tyConParser
  tyArgs <- many nameParser
  myString "="
  DataType tyName tyArgs <$> manyTypeConstructors

--------

manyTypeConstructors :: Parser (Map TyCon [Type NullUnit])
manyTypeConstructors = do
  tyCons <-
    sepBy
      oneTypeConstructor
      (myString "|")
  pure (mconcat tyCons)

-----

oneTypeConstructor :: Parser (Map TyCon [Type NullUnit])
oneTypeConstructor = do
  name <- tyConParser
  args <-
    some typeDeclParser'
      <|> pure mempty
  let argsWithNoType = ($> NullUnit) <$> args
  pure (M.singleton name argsWithNoType)

-----
