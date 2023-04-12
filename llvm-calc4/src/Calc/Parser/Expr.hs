{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Expr (exprParser) where

import Calc.Parser.Identifier
import Calc.Parser.Pattern
import Calc.Parser.Primitives
import Calc.Parser.Shared
import Calc.Parser.Types
import Calc.Types.Annotation
import Calc.Types.Expr
import Control.Monad.Combinators.Expr
import qualified Data.List.NonEmpty as NE
import Data.Text
import Text.Megaparsec

exprParser :: Parser (Expr Annotation)
exprParser = addLocation (makeExprParser exprPart table) <?> "expression"

exprPart :: Parser (Expr Annotation)
exprPart =
  try tupleParser
    <|> inBrackets (addLocation exprParser)
    <|> patternMatchParser
    <|> primExprParser
    <|> ifParser
    <|> try applyParser
    <|> varParser
    <?> "term"

table :: [[Operator Parser (Expr Annotation)]]
table =
  [ [binary "*" (EInfix mempty OpMultiply)],
    [ binary "+" (EInfix mempty OpAdd),
      binary "-" (EInfix mempty OpSubtract)
    ],
    [binary "==" (EInfix mempty OpEquals)]
  ]

binary :: Text -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ stringLiteral name)

ifParser :: Parser (Expr Annotation)
ifParser = addLocation $ do
  stringLiteral "if"
  predExpr <- exprParser
  stringLiteral "then"
  thenExpr <- exprParser
  stringLiteral "else"
  EIf mempty predExpr thenExpr <$> exprParser

varParser :: Parser (Expr Annotation)
varParser = addLocation $ EVar mempty <$> identifierParser

applyParser :: Parser (Expr Annotation)
applyParser = addLocation $ do
  fnName <- functionNameParser
  stringLiteral "("
  args <- sepBy exprParser (stringLiteral ",")
  stringLiteral ")"
  pure (EApply mempty fnName args)

tupleParser :: Parser (Expr Annotation)
tupleParser = label "tuple" $
  addLocation $ do
    _ <- stringLiteral "("
    neArgs <- NE.fromList <$> sepBy1 exprParser (stringLiteral ",")
    neTail <- case NE.nonEmpty (NE.tail neArgs) of
      Just ne -> pure ne
      _ -> fail "Expected at least two items in a tuple"
    _ <- stringLiteral ")"
    pure (ETuple mempty (NE.head neArgs) neTail)

-----

patternMatchParser :: Parser ParserExpr
patternMatchParser = addLocation $ do
  matchExpr <- matchExprWithParser
  patterns <-
    try patternMatchesParser
      <|> pure <$> patternCaseParser
  case NE.nonEmpty patterns of
    (Just nePatterns) -> pure $ EPatternMatch mempty matchExpr nePatterns
    _ -> error "need at least one pattern"

matchExprWithParser :: Parser ParserExpr
matchExprWithParser = do
  stringLiteral "case"
  sumExpr <- exprParser
  stringLiteral "of"
  pure sumExpr

patternMatchesParser :: Parser [(ParserPattern, ParserExpr)]
patternMatchesParser =
  sepBy
    patternCaseParser
    (stringLiteral "|")

patternCaseParser :: Parser (ParserPattern, ParserExpr)
patternCaseParser = do
  pat <- orInBrackets patternParser
  stringLiteral "->"
  patExpr <- exprParser
  pure (pat, patExpr)
