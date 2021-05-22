{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Pattern
  ( patternParser,
    ParserPattern,
  )
where

import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Parser.Literal
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Text.Megaparsec
import Text.Megaparsec.Char

type ParserPattern = Pattern Name Annotation

patternParser :: Parser ParserPattern
patternParser =
  try pairParser
    <|> try wildcardParser
    <|> try variableParser
    <|> try litParser

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

----

pairParser :: Parser ParserPattern
pairParser = withLocation (\loc (one, two) -> PPair loc one two) $ do
  _ <- string "("
  one <- patternParser
  _ <- literalWithSpace ","
  two <- patternParser
  _ <- string ")"
  pure (one, two)

----

litParser :: Parser ParserPattern
litParser = withLocation PLit lit
  where
    lit =
      try integerLiteral
        <|> try stringLiteral
        <|> trueParser
        <|> falseParser
