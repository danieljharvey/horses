{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Core.Parser.TypeDecl
  ( typeDeclParser,
    parseTypeDecl,
  )
where

import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Language.Mimsa.Core.Parser.Identifiers
import Language.Mimsa.Core.Parser.Lexeme
import Language.Mimsa.Core.Parser.MonoType
import Language.Mimsa.Core.Parser.Types
import Language.Mimsa.Core.Types.AST
import Language.Mimsa.Core.Types.Identifiers (TyCon)
import Language.Mimsa.Core.Types.Type
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
  tyName <- typeNameParser
  pure (DataType tyName mempty mempty)

-- it's your more complex cases
typeDeclParserWithCons :: Parser DataType
typeDeclParserWithCons = do
  myString "type"
  tyName <- typeNameParser
  tyArgs <- many nameParser
  myString "="
  DataType tyName tyArgs <$> manyTypeConstructors

--------

manyTypeConstructors :: Parser (Map TyCon [Type ()])
manyTypeConstructors = do
  tyCons <-
    sepBy
      oneTypeConstructor
      (myString "|")
  pure (mconcat tyCons)

-----

oneTypeConstructor :: Parser (Map TyCon [Type ()])
oneTypeConstructor = do
  name <- tyConParser
  args <-
    some typeDeclParser'
      <|> pure mempty
  let argsWithNoType = ($> ()) <$> args
  pure (M.singleton name argsWithNoType)

-----
