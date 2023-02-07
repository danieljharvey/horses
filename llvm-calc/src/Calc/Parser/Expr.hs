{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Expr (exprParser) where

import Calc.Parser.Primitives
import Calc.Parser.Shared
import Calc.Parser.Types
import Calc.Types.Annotation
import Calc.Types.Expr
import Control.Monad.Combinators.Expr
import Data.Text
import Text.Megaparsec

exprParser :: Parser (Expr Annotation)
exprParser = addLocation (makeExprParser exprPart table) <?> "expression"

exprPart :: Parser (Expr Annotation)
exprPart =
  inBrackets (addLocation exprParser)
    <|> primParser
    <?> "term"

table :: [[Operator Parser (Expr Annotation)]]
table =
  [ [binary "*" (EInfix mempty OpMultiply)],
    [ binary "+" (EInfix mempty OpAdd),
      binary "-" (EInfix mempty OpSubtract)
    ]
  ]

binary :: Text -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ stringLiteral name)
