{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser.Identifiers
  ( identifierParser,
    varParser,
    constructorParser,
    constructorParserInternal,
    globalParser,
    moduleNameParser,
    typeNameParser,
  )
where

import Control.Monad
import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Void
import Smol.Core.Parser.Shared
import Smol.Core.Types
import Smol.Core.Types.Module.ModuleName
import Text.Megaparsec

type Parser = Parsec Void Text

type ParserExpr = Expr ParseDep Annotation

protectedNames :: Set Text
protectedNames =
  S.fromList
    [ "let",
      "in",
      "if",
      "then",
      "else",
      "type",
      "case",
      "of",
      "infix",
      "True",
      "False",
      "Nat",
      "Unit",
      "def",
      "export",
      "import",
      "test"
    ]

filterProtectedNames :: Text -> Maybe Text
filterProtectedNames tx =
  if S.member tx protectedNames
    then Nothing
    else Just tx

-- `dog`, `log`, `a`, `Prelude.id`
varParser :: Parser ParserExpr
varParser =
  try namespacedVarParser <|> try plainVarParser

-- `dog`, `log`, `a`
plainVarParser :: Parser ParserExpr
plainVarParser =
  myLexeme (withLocation (\ann var ->
      EVar ann  (ParseDep var Nothing)) identifierParser)

-- `Dog.log`, `Maybe.fmap`
namespacedVarParser :: Parser ParserExpr
namespacedVarParser =
  let inner = do
        (var, mName) <- withNamespace identifierParser
        pure $ EVar mempty (ParseDep var (Just mName))
   in myLexeme (addLocation inner)


-- `dog!`, `log!`, `a!`
globalParser :: Parser ParserExpr
globalParser =
  myLexeme (withLocation EGlobal $ identifierParser <* myString "!")

---------------------

-- `Dog`, `Log`, `A`
constructorParser :: Parser ParserExpr
constructorParser =
  try namespacedConstructorParser <|> try plainConstructorParser

plainConstructorParser :: Parser ParserExpr
plainConstructorParser =
  myLexeme (withLocation EConstructor (emptyParseDep <$> constructorParserInternal))

-- `Maybe.Just`, `Either.Right`
namespacedConstructorParser :: Parser ParserExpr
namespacedConstructorParser =
  let inner = do
        (cons, mName) <- withNamespace constructorParserInternal
        pure $ EConstructor mempty (ParseDep cons (Just mName))
   in myLexeme (addLocation inner)

-----------------------

-- `Maybe`, `Either` etc
typeNameParser :: Parser TypeName
typeNameParser = myLexeme (TypeName <$> constructorParserInternal)

-- identifier

identifier :: Parser Text
identifier = takeWhile1P (Just "variable name") Char.isAlphaNum

identifierParser :: Parser Identifier
identifierParser =
  myLexeme identifierParserInternal

-- use this when you are going to wrap myLexeme yourself
identifierParserInternal :: Parser Identifier
identifierParserInternal =
  maybePred
    identifier
    (filterProtectedNames >=> safeMkIdentifier)

-- constructor

constructor :: Parser Text
constructor = takeWhile1P (Just "constructor") Char.isAlphaNum

-- use this when you are going to wrap myLexeme yourself
constructorParserInternal :: Parser Constructor
constructorParserInternal =
  maybePred
    constructor
    (filterProtectedNames >=> safeMkConstructor)

----

moduleName :: Parser Text
moduleName = takeWhile1P (Just "constructor") Char.isAlphaNum

moduleNameParser :: Parser ModuleName
moduleNameParser =
  myLexeme $
    maybePred
      moduleName
      (filterProtectedNames >=> safeMkModuleName)

--------

withNamespace :: Parser a -> Parser (a, ModuleName)
withNamespace p = do
        mName <- moduleNameParser
        myString "."
        a <- p
        pure (a, mName)



