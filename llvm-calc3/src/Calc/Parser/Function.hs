{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Function (functionParser) where

import Calc.Parser.Expr
import Calc.Parser.Identifier
import Calc.Parser.Shared
import Calc.Parser.Type
import Calc.Parser.Types
import Calc.Types.Annotation
import Calc.Types.Function
import Calc.Types.Identifier
import Calc.Types.Type
import Text.Megaparsec

functionParser :: Parser (Function Annotation)
functionParser =
  withLocation (\ann (args, fnName, expr) -> Function ann args fnName expr) innerParser
  where
    innerParser = do
      stringLiteral "function"
      (Identifier fnName) <- identifierParser
      stringLiteral "("
      args <- sepBy argTypeParser (stringLiteral ",")
      stringLiteral ")"
      stringLiteral "{"
      expr <- exprParser
      stringLiteral "}"
      pure (args, FunctionName fnName, expr)

argTypeParser :: Parser (ArgumentName, Type Annotation)
argTypeParser = do
  (Identifier arg) <- identifierParser
  stringLiteral ":"
  (,) (ArgumentName arg) <$> typeParser
