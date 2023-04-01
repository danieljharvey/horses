{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Core.Parser.Identifiers
  ( varParser,
    nameParser,
    globalParser,
    nameParserInt,
    infixOpParser,
    tyConParser,
    typedHoleParser,
    constructorParser,
    moduleNameParser,
    typeNameParser,
  )
where

import Control.Monad ((>=>))
import Data.Char as Char
import Data.Text (Text)
import Language.Mimsa.Core.Parser.Helpers
import Language.Mimsa.Core.Parser.Lexeme
import Language.Mimsa.Core.Parser.Types
import Language.Mimsa.Core.Types.AST
import Language.Mimsa.Core.Types.Identifiers
import Language.Mimsa.Core.Types.Module.ModuleName
import Text.Megaparsec

----

varParser :: Parser ParserExpr
varParser =
  try namespacedVarParser <|> try plainVarParser

-- `dog`, `log`, `a`
plainVarParser :: Parser ParserExpr
plainVarParser =
  myLexeme (withLocation (`MyVar` Nothing) nameParser)

-- `Dog.log`, `Maybe.fmap`
namespacedVarParser :: Parser ParserExpr
namespacedVarParser =
  let inner = do
        mName <- moduleNameParser
        myString "."
        MyVar mempty (Just mName) <$> nameParser
   in myLexeme (addLocation inner)

---

identifier :: Parser Text
identifier = takeWhile1P (Just "variable name") Char.isAlphaNum

---

-- `dog?`, `log?`, `a?`
globalParser :: Parser ParserExpr
globalParser =
  myLexeme (withLocation MyGlobal $ globalNameParser <* myString "?")
    where
      globalNameParser
          = GlobalName <$> identifier

---

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
constructorParser =
  try namespacedConstructorParser
    <|> try plainConstructorParser

plainConstructorParser :: Parser ParserExpr
plainConstructorParser = withLocation (`MyConstructor` Nothing) tyConParser

namespacedConstructorParser :: Parser ParserExpr
namespacedConstructorParser =
  let inner = do
        mName <- moduleNameParser
        myString "."
        MyConstructor mempty (Just mName) <$> tyConParser
   in myLexeme (addLocation inner)

--

tyConParser :: Parser TyCon
tyConParser =
  myLexeme $
    maybePred
      identifier
      (filterProtectedNames >=> safeMkTyCon)

---

typeNameParser :: Parser TypeName
typeNameParser =
  myLexeme $
    maybePred
      identifier
      (filterProtectedNames >=> safeMkTypeName)

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
