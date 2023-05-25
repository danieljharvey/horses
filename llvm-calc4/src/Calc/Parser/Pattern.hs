{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Pattern
  ( patternParser,
    ParserPattern,
  )
where

import Calc.Parser.Identifier
import Calc.Parser.Primitives
import Calc.Parser.Shared
import Calc.Parser.Types
import Calc.Types
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec
import Text.Megaparsec.Char

type ParserPattern = Pattern Annotation

patternParser :: Parser ParserPattern
patternParser =
  label
    "pattern match"
    ( orInBrackets
        ( try patTupleParser
            <|> try patWildcardParser
            <|> try patVariableParser
            <|> patLitParser
        )
    )

----

patWildcardParser :: Parser ParserPattern
patWildcardParser =
  myLexeme $
    withLocation
      (\loc _ -> PWildcard loc)
      (string "_")

----

patVariableParser :: Parser ParserPattern
patVariableParser =
  myLexeme $ withLocation PVar identifierParser

----

patTupleParser :: Parser ParserPattern
patTupleParser = label "tuple" $
  withLocation (\loc (pHead, pTail) -> PTuple loc pHead pTail) $ do
    _ <- stringLiteral "("
    neArgs <- NE.fromList <$> sepBy1 patternParser (stringLiteral ",")
    neTail <- case NE.nonEmpty (NE.tail neArgs) of
      Just ne -> pure ne
      _ -> fail "Expected at least two items in a tuple"
    _ <- stringLiteral ")"
    pure (NE.head neArgs, neTail)

----

patLitParser :: Parser ParserPattern
patLitParser = myLexeme $ withLocation PLiteral primParser
