{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Literal
  ( literalParser,
    stringLiteral,
    trueParser,
    falseParser,
    integerLiteral,
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
    <|> try intParser
    <|> try stringParser

----

integerLiteral :: Parser Literal
integerLiteral = MyInt <$> L.signed space L.decimal

intParser :: Parser ParserExpr
intParser = withLocation MyLiteral integerLiteral

---

boolParser :: Parser ParserExpr
boolParser = withLocation MyLiteral (trueParser <|> falseParser)

trueParser :: Parser Literal
trueParser = string "True" $> MyBool True

falseParser :: Parser Literal
falseParser = string "False" $> MyBool False

-----

stringLiteral :: Parser Literal
stringLiteral = do
  chr <- char '\"' *> manyTill L.charLiteral (char '\"')
  pure (MyString $ StringType $ T.pack chr)

stringParser :: Parser ParserExpr
stringParser =
  withLocation
    MyLiteral
    stringLiteral

-----
