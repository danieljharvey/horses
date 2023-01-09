{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser.Identifiers
  ( identifierParser,
    varParser,
    constructorParser,
    constructorParserInternal,
    globalParser,
  )
where

import Control.Monad
import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Void
import Smol.Core.Parser.Shared
import Text.Megaparsec
import Smol.Core.Types
import Smol.Core.Types.Constructor
import Smol.Core.Types.Identifier

type Parser = Parsec Void Text

type ParserExpr = Expr Annotation

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

-- `dog`, `log`, `a`
varParser :: Parser ParserExpr
varParser =
  myLexeme (withLocation EVar identifierParser)

-- `dog!`, `log!`, `a!`
globalParser :: Parser ParserExpr
globalParser =
  myLexeme (withLocation EGlobal $ identifierParser <* myString "!")

-- `Dog`, `Log`, `A`
constructorParser :: Parser ParserExpr
constructorParser =
  myLexeme (withLocation EConstructor constructorParserInternal)

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
