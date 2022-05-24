{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Identifiers
  ( varParser,
    nameParser,
    nameParserInt,
    infixOpParser,
    tyConParser,
    typedHoleParser,
    constructorParser,
  )
where

import Control.Monad ((>=>))
import Data.Char as Char
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Lexeme
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Text.Megaparsec

----

varParser :: Parser ParserExpr
varParser = myLexeme (withLocation MyVar nameParser)

---

identifier :: Parser Text
identifier = takeWhile1P (Just "variable name") Char.isAlphaNum

nameParser :: Parser Name
nameParser =
  myLexeme nameParserInt

-- use this when you are going to wrap myLexeme yourself
nameParserInt :: Parser Name
nameParserInt =
  maybePred
    identifier
    (filterProtectedNames >=> safeMkName)

---

constructorParser :: Parser ParserExpr
constructorParser = withLocation MyConstructor tyConParser

tyConParser :: Parser TyCon
tyConParser =
  myLexeme $
    maybePred
      identifier
      (filterProtectedNames >=> safeMkTyCon)

-----

typedHoleParser :: Parser ParserExpr
typedHoleParser =
  withLocation
    MyTypedHole
    ( do
        myString "?"
        nameParser
    )

-----

infixIdentifier :: Parser Text
infixIdentifier = takeWhile1P (Just "infix operator") (not . Char.isSpace)

infixOpParser :: Parser InfixOp
infixOpParser =
  myLexeme
    ( maybePred
        infixIdentifier
        (filterProtectedOperators >=> safeMkInfixOp)
    )
