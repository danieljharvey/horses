{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Parser where

import Control.Applicative ((<|>))
import Language.Mimsa.Repl.Types
import Language.Mimsa.Syntax

replParser :: Parser ReplAction
replParser = evaluateParser <|> bindParser <|> listBindingsParser

evaluateParser :: Parser ReplAction
evaluateParser =
  Evaluate
    <$> right
      (thenSpace (literal ":info"))
      expressionParser

bindParser :: Parser ReplAction
bindParser =
  Bind
    <$> right (thenSpace (literal ":bind")) (thenSpace nameParser)
    <*> right (thenSpace (literal "=")) expressionParser

listBindingsParser :: Parser ReplAction
listBindingsParser = ListBindings <$ literal ":list"
