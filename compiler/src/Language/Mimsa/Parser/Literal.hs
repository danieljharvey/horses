{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Literal
  ( literalParser,
    stringLiteral,
  )
where

import Data.Functor (($>))
import qualified Data.Text as T
import Language.Mimsa.Parser.Helpers
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
boolParser = withLocation MyLiteral (trueParser <|> falseParser)

trueParser :: Parser Literal
trueParser = string "True" $> MyBool True

falseParser :: Parser Literal
falseParser = string "False" $> MyBool False

-----

unitParser :: Parser ParserExpr
unitParser = withLocation MyLiteral (string "Unit" $> MyUnit ())

-----

intParser :: Parser ParserExpr
intParser = withLocation MyLiteral (MyInt <$> integerParser)

-----

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

stringParser :: Parser ParserExpr
stringParser =
  withLocation
    MyLiteral
    ( MyString
        . StringType
        . T.pack
        <$> stringLiteral
    )

-----
