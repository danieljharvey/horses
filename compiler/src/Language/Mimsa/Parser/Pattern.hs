{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Pattern
  ( patternParser,
    ParserPattern,
  )
where

import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Text.Megaparsec
import Text.Megaparsec.Char

type ParserPattern = Pattern Name Annotation

patternParser :: Parser ParserPattern
patternParser =
  try wildcardParser
    <|> try variableParser

----

wildcardParser :: Parser ParserPattern
wildcardParser =
  withLocation
    (\loc _ -> PWildcard loc)
    (string "_")

----

variableParser :: Parser ParserPattern
variableParser =
  withLocation PVar nameParser
