{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser.Primitives
  ( primParser,
    typeLiteralParser,
    intPrimParser,
    truePrimParser,
    falsePrimParser,
    stringPrimParser,
    unitParser,
    textPrim,
  )
where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set.NonEmpty as NES
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Smol.Core.Parser.Shared
import Smol.Core.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

primParser :: Parser Prim
primParser =
  myLexeme
    ( intPrimParser
        <|> truePrimParser
        <|> falsePrimParser
        <|> PUnit <$ unitParser
        <|> stringPrimParser
    )

typeLiteralParser :: Parser TypeLiteral
typeLiteralParser =
  myLexeme
    ( TLInt <$> multiIntParser
        <|> TLBool <$> trueParser
        <|> TLBool <$> falseParser
        <|> TLUnit <$ unitParser
        <|> TLString <$> multiStringParser
    )

-- 2, -2, +2
intPrimParser :: Parser Prim
intPrimParser = PInt <$> intParser

intParser :: Parser Integer
intParser =
  L.signed (string "" $> ()) L.decimal

unionParser :: (Ord a) => Parser a -> Parser (NES.NESet a)
unionParser parseA = do
  ints <-
    sepBy1
      (myLexeme parseA)
      (myString "|")
  pure (NES.fromList (NE.fromList ints))

multiIntParser :: Parser (NES.NESet Integer)
multiIntParser = unionParser intParser

---

truePrimParser :: Parser Prim
truePrimParser = PBool <$> trueParser

trueParser :: Parser Bool
trueParser = myString "True" $> True

falsePrimParser :: Parser Prim
falsePrimParser = PBool <$> falseParser

falseParser :: Parser Bool
falseParser = myString "False" $> False

----

unitParser :: Parser ()
unitParser = myString "Unit" $> ()

---

stringPrimParser :: Parser Prim
stringPrimParser =
  PString <$> textPrim

textPrim :: Parser Text
textPrim = T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

multiStringParser :: Parser (NES.NESet Text)
multiStringParser = unionParser textPrim
