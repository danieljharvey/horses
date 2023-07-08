{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser.Identifiers
  ( identifierParser,
    varParser,
    constructorParser,
    constructorParserInternal,
    innerConstructorParser,
    globalParser,
    moduleNameParser,
    typeNameParser,
    plainTypeNameParser,
    testNameParser,
  )
where

import Control.Monad
import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Void
import Smol.Core.Modules.Types.ModuleName
import Smol.Core.Modules.Types.TestName
import Smol.Core.Parser.Primitives (textPrim)
import Smol.Core.Parser.Shared
import Smol.Core.Types
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
      "using",
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
  myLexeme
    ( withLocation
        ( \ann var ->
            EVar ann (ParseDep var Nothing)
        )
        identifierParser
    )

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

-- just the constructor (you'll need to add Lexeme, location etc)
innerConstructorParser :: Parser (ParseDep Constructor)
innerConstructorParser =
  try withModule <|> try withoutModule
  where
    withModule = do
      (cons, mName) <- withNamespace constructorParserInternal
      pure $ ParseDep cons (Just mName)
    withoutModule =
      emptyParseDep <$> constructorParserInternal

-----------------------

typeNameParser :: Parser (ParseDep TypeName)
typeNameParser =
  try namespacedTypeNameParser <|> try (emptyParseDep <$> plainTypeNameParser)

-- `Maybe`, `Either` etc
plainTypeNameParser :: Parser TypeName
plainTypeNameParser = myLexeme (TypeName <$> constructorParserInternal)

namespacedTypeNameParser :: Parser (ParseDep TypeName)
namespacedTypeNameParser =
  let inner = do
        (cons, mName) <- withNamespace (TypeName <$> constructorParserInternal)
        pure $ ParseDep cons (Just mName)
   in myLexeme inner

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

-----

testNameParser :: Parser TestName
testNameParser = myLexeme $ TestName <$> textPrim
