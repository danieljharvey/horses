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
import Language.Mimsa.Parser.Lexeme
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

-- 2, -2, +2
integerLiteral :: Parser Literal
integerLiteral =
  MyInt <$> L.signed (string "" $> ()) L.decimal

intParser :: Parser ParserExpr
intParser = myLexeme (withLocation MyLiteral integerLiteral)

---

boolParser :: Parser ParserExpr
boolParser =
  myLexeme
    ( withLocation
        MyLiteral
        (trueParser <|> falseParser)
    )

trueParser :: Parser Literal
trueParser = myString "True" $> MyBool True

falseParser :: Parser Literal
falseParser = myString "False" $> MyBool False

-----

stringLiteral :: Parser Literal
stringLiteral = do
  chr <- char '\"' *> manyTill L.charLiteral (char '\"')
  pure (MyString $ StringType $ T.pack chr)

stringParser :: Parser ParserExpr
stringParser =
  myLexeme
    ( withLocation
        MyLiteral
        stringLiteral
    )
