{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser.Primitives
  ( primParser,
    typeLiteralParser,
    natPrimParser,
    intPrimParser,
    truePrimParser,
    falsePrimParser,
    unitParser,
  )
where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set.NonEmpty as NES
import Data.Text (Text)
import Data.Void
import GHC.Num.Natural
import Smol.Core.Parser.Shared
import Smol.Core.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

primParser :: Parser Prim
primParser =
  myLexeme
    ( natPrimParser
        <|> intPrimParser
        <|> truePrimParser
        <|> falsePrimParser
        <|> PUnit <$ unitParser
    )

typeLiteralParser :: Parser TypeLiteral
typeLiteralParser =
  myLexeme
    ( TLInt <$> multiIntParser
        <|> TLBool <$> trueParser
        <|> TLBool <$> falseParser
        <|> TLUnit <$ unitParser
    )

-- 0, 1, 2
natPrimParser :: Parser Prim
natPrimParser = PNat <$> natParser

natParser :: Parser Natural
natParser =
  L.decimal

----

-- 2, -2, +2
intPrimParser :: Parser Prim
intPrimParser = PInt <$> intParser

intParser :: Parser Integer
intParser =
  L.signed (string "" $> ()) L.decimal

multiIntParser :: Parser (NES.NESet Integer)
multiIntParser = do
  ints <-
    sepBy1
      (myLexeme intParser)
      (myString "|")
  pure (NES.fromList (NE.fromList ints))

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

{-
     textPrim :: Parser Text
textPrim = T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))
-}

{-
stringPrim :: Parser Prim
stringPrim =
  MyString . StringType <$> textPrim

stringParser :: Parser ParserExpr
stringParser =
  myLexeme
    ( withLocation
        EPrim
        stringPrim
    )
-}

----
