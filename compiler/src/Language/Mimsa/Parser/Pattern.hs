{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Pattern
  ( patternParser,
    ParserPattern,
  )
where

import Data.Either (partitionEithers)
import qualified Data.Map as M
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifiers (moduleNameParser, nameParser, tyConParser)
import Language.Mimsa.Parser.Lexeme
import Language.Mimsa.Parser.Literal
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Text.Megaparsec
import Text.Megaparsec.Char

type ParserPattern = Pattern Name Annotation

patternParser :: Parser ParserPattern
patternParser =
  label
    "pattern match"
    ( orInBrackets
        ( try stringParser
            <|> try pairParser
            <|> try wildcardParser
            <|> try variableParser
            <|> try litParser
            <|> try recordParser
            <|> try constructorParser
            <|> try arrayParser
        )
    )

----

wildcardParser :: Parser ParserPattern
wildcardParser =
  myLexeme $
    withLocation
      (\loc _ -> PWildcard loc)
      (string "_")

----

variableParser :: Parser ParserPattern
variableParser =
  myLexeme $ withLocation PVar nameParser

----

pairParser :: Parser ParserPattern
pairParser = withLocation (\loc (one, two) -> PPair loc one two) $ do
  _ <- myString "("
  one <- patternParser
  _ <- myString ","
  two <- patternParser
  _ <- myString ")"
  pure (one, two)

----

litParser :: Parser ParserPattern
litParser = withLocation PLit lit
  where
    lit =
      try (myLexeme integerLiteral)
        <|> try (myLexeme stringLiteral)
        <|> trueParser
        <|> falseParser

---

recordParser :: Parser ParserPattern
recordParser = withLocation PRecord $ do
  let itemParser =
        try recordItemParser
          <|> punnedRecordItemParser
  _ <- myString "{"
  args <- sepBy itemParser (myString ",")
  _ <- myString "}"
  pure (M.fromList args)

recordItemParser :: Parser (Name, ParserPattern)
recordItemParser = do
  name <- nameParser
  myString ":"
  expr <- patternParser
  pure (name, expr)

punnedRecordItemParser :: Parser (Name, ParserPattern)
punnedRecordItemParser = do
  name <- nameParser
  pure (name, PVar mempty name)

---

argsParser :: Parser [ParserPattern]
argsParser = try someP <|> pure []
  where
    someP = some patternParser

----

constructorParser :: Parser ParserPattern
constructorParser =
  try namespacedConstructorParser
    <|> try plainConstructorParser

plainConstructorParser :: Parser ParserPattern
plainConstructorParser =
  let parser = do
        cons <- tyConParser
        args <- try argsParser
        pure (cons, args)
   in withLocation
        ( \loc (cons, args) ->
            PConstructor loc Nothing cons args
        )
        parser

namespacedConstructorParser :: Parser ParserPattern
namespacedConstructorParser =
  let inner = do
        mName <- moduleNameParser
        myString "."
        tyCon <- tyConParser
        args <- try argsParser
        pure (mName, tyCon, args)
   in myLexeme
        ( withLocation
            ( \loc (mName, tyCon, args) ->
                PConstructor loc (Just mName) tyCon args
            )
            inner
        )

---

arrayParser :: Parser ParserPattern
arrayParser =
  let itemParser =
        try (Right <$> patternParser)
          <|> try (Left <$> spreadParser)
          <|> fail "Expected pattern or a spread operator"
      parser = do
        myString "["
        args <- sepBy itemParser (myString ",")
        myString "]"
        case getParts args of
          Right parts -> pure parts
          Left e -> fail e
   in withLocation (\loc (as, spread) -> PArray loc as spread) parser

getParts ::
  [Either (Spread Name Annotation) (Pattern Name Annotation)] ->
  Either String ([Pattern Name Annotation], Spread Name Annotation)
getParts as = case reverse as of
  ((Left spr) : rest) ->
    case partitionEithers rest of
      ([], pats) | not (null pats) -> pure (reverse pats, spr)
      ([], _) -> Left "There must be at least one pattern to use a spread"
      _ -> Left "Cannot have more than one spread in an array pattern"
  es -> case partitionEithers es of
    ([], pats) -> pure (reverse pats, NoSpread)
    _ -> Left "Cannot have more than one spread in an array pattern"

---

spreadParser :: Parser (Spread Name Annotation)
spreadParser =
  try spreadValueParser
    <|> try spreadWildcardParser

spreadWildcardParser :: Parser (Spread Name Annotation)
spreadWildcardParser =
  let parser =
        myString "..."
   in withLocation (\loc _ -> SpreadWildcard loc) parser

spreadValueParser :: Parser (Spread Name Annotation)
spreadValueParser =
  let parser = do
        myString "..."
        nameParser
   in withLocation SpreadValue parser

---

stringParser :: Parser (Pattern Name Annotation)
stringParser =
  let parser = do
        a <- stringPartParser
        myString "++"
        as <- stringPartParser
        pure (a, as)
   in withLocation (\loc (a, as) -> PString loc a as) parser

stringPartParser :: Parser (StringPart Name Annotation)
stringPartParser =
  try stringWildcard <|> try stringValue

stringWildcard :: Parser (StringPart Name Annotation)
stringWildcard =
  let parser = myString "_"
   in withLocation (\loc _ -> StrWildcard loc) parser

stringValue :: Parser (StringPart Name Annotation)
stringValue =
  withLocation StrValue nameParser
