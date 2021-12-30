{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Identifiers
  ( varParser,
    nameParser,
    infixOpParser,
    tyConParser,
    typeNameParser,
    typedHoleParser,
    constructorParser,
  )
where

import Control.Monad ((>=>))
import Data.Char as Char
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Text.Megaparsec
import Text.Megaparsec.Char

----

varParser :: Parser ParserExpr
varParser = withLocation MyVar nameParser

---

identifier :: Parser Text
identifier = takeWhile1P (Just "variable name") Char.isAlphaNum

nameParser :: Parser Name
nameParser =
  maybePred
    identifier
    (filterProjectedNames >=> safeMkName)

---

constructorParser :: Parser ParserExpr
constructorParser = withLocation MyConstructor tyConParser

tyConParser :: Parser TyCon
tyConParser =
  maybePred
    identifier
    (filterProjectedNames >=> safeMkTyCon)

typeNameParser :: Parser TypeName
typeNameParser =
  maybePred
    identifier
    (filterProjectedNames >=> safeMkTypeName)

-----

typedHoleParser :: Parser ParserExpr
typedHoleParser =
  withLocation
    MyTypedHole
    ( do
        _ <- string "?"
        nameParser
    )

-----

infixIdentifier :: Parser Text
infixIdentifier = takeWhile1P (Just "infix operator") (not . Char.isSpace)

infixOpParser :: Parser InfixOp
infixOpParser =
  maybePred
    infixIdentifier
    (filterProtectedOperators >=> safeMkInfixOp)
