{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Pattern
  ( patternParser,
    ParserPattern,
  )
where

import qualified Data.Map as M
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifiers (nameParser, tyConParser)
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
    <|> try recordParser
    <|> try constructorParser

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

---

recordParser :: Parser ParserPattern
recordParser = withLocation PRecord $ do
  _ <- string "{"
  _ <- space
  args <- sepBy (withOptionalSpace recordItemParser) (literalWithSpace ",")
  _ <- space
  _ <- string "}"
  pure (M.fromList args)

recordItemParser :: Parser (Name, ParserPattern)
recordItemParser = do
  name <- nameParser
  literalWithSpace ":"
  expr <- withOptionalSpace patternParser
  pure (name, expr)

---

argsParser :: Parser [ParserPattern]
argsParser = try someP <|> pure []
  where
    someP = do
      _ <- space1
      sepBy1
        patternParser
        space1

constructorParser :: Parser ParserPattern
constructorParser =
  let parser = do
        cons <- tyConParser
        args <- try argsParser
        pure (cons, args)
   in withLocation
        ( \loc (cons, args) ->
            PConstructor loc cons args
        )
        parser
