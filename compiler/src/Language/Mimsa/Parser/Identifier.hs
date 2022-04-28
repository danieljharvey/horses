{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Identifier
  ( identifierParser,
  )
where

import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Parser.Lexeme
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

----

identifierParser :: Parser (Identifier Name Annotation)
identifierParser =
  plainIdentifierParser

plainIdentifierParser :: Parser (Identifier Name Annotation)
plainIdentifierParser =
  myLexeme
    ( withLocation
        Identifier
        nameParserInt
    )
