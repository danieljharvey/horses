{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Parser where

import Control.Applicative ((<|>))
import Language.Mimsa.Repl.Types
import Language.Mimsa.Syntax

replParser :: Parser ReplAction
replParser =
  helpParser
    <|> infoParser
    <|> bindParser
    <|> listBindingsParser
    <|> evalParser

helpParser :: Parser ReplAction
helpParser = Help <$ literal ":help"

infoParser :: Parser ReplAction
infoParser =
  Info
    <$> right
      (thenSpace (literal ":info"))
      expressionParser

evalParser :: Parser ReplAction
evalParser =
  Evaluate
    <$> expressionParser

bindParser :: Parser ReplAction
bindParser =
  Bind
    <$> right (thenSpace (literal ":bind")) (thenSpace nameParser)
    <*> right (thenSpace (literal "=")) expressionParser

listBindingsParser :: Parser ReplAction
listBindingsParser = ListBindings <$ literal ":list"
