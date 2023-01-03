{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Core.Parser.Literal
  ( literalParser,
    testNameParser,
    stringLiteral,
    trueParser,
    falseParser,
    integerLiteral,
    natParser,
  )
where

import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Natural
import Language.Mimsa.Core.Parser.Helpers
import Language.Mimsa.Core.Parser.Lexeme
import Language.Mimsa.Core.Parser.Types
import Language.Mimsa.Core.Types.AST
import Language.Mimsa.Core.Types.Tests
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

natParser :: Parser Natural
natParser = L.decimal

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

textLiteral :: Parser Text
textLiteral = T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

stringLiteral :: Parser Literal
stringLiteral =
  MyString . StringType <$> textLiteral

stringParser :: Parser ParserExpr
stringParser =
  myLexeme
    ( withLocation
        MyLiteral
        stringLiteral
    )

testNameParser :: Parser TestName
testNameParser = TestName <$> myLexeme textLiteral
