{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Literal
  ( literalParser,
  )
where

import Data.Functor (($>))
import qualified Data.Text as T
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

literalParser :: Parser ParserExpr
literalParser =
  try boolParser
    <|> try unitParser
    <|> try intParser
    <|> try stringParser

----

integerParser :: Parser Int
integerParser = L.signed space L.decimal

---

boolParser :: Parser ParserExpr
boolParser = trueParser <|> falseParser

trueParser :: Parser ParserExpr
trueParser = string "True" $> MyLiteral mempty (MyBool True)

falseParser :: Parser ParserExpr
falseParser = string "False" $> MyLiteral mempty (MyBool False)

-----

unitParser :: Parser ParserExpr
unitParser = string "Unit" $> MyLiteral mempty MyUnit

-----

intParser :: Parser ParserExpr
intParser = MyLiteral mempty . MyInt <$> integerParser

-----

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

stringParser :: Parser ParserExpr
stringParser =
  MyLiteral mempty . MyString
    . StringType
    . T.pack
    <$> stringLiteral
-----
