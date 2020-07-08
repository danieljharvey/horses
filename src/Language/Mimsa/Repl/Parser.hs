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
    <|> treeParser
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

treeParser :: Parser ReplAction
treeParser = Tree <$> right (thenSpace (literal ":tree")) expressionParser

bindParser :: Parser ReplAction
bindParser =
  Bind
    <$> right (thenSpace (literal ":bind")) (thenSpace nameParser)
    <*> right (thenSpace (literal "=")) expressionParser

listBindingsParser :: Parser ReplAction
listBindingsParser = ListBindings <$ literal ":list"
