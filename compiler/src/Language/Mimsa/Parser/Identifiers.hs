{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Identifiers
  ( varParser,
    nameParser,
    nameParserInt,
    infixOpParser,
    tyConParser,
    typedHoleParser,
    constructorParser,
    moduleNameParser
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
import Language.Mimsa.Types.Modules.ModuleName

----

varParser :: Parser ParserExpr
varParser = 
  myLexeme (withLocation (\ann -> MyVar ann Nothing) nameParser)

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

---

moduleNameParser :: Parser ModuleName
moduleNameParser =
  myLexeme $
    maybePred
      identifier
      (filterProtectedNames >=> safeMkModuleName)


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
