{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Parser
  ( replParser,
  )
where

import Language.Mimsa.Parser
import Language.Mimsa.Repl.Types
import Language.Mimsa.Types.AST
import Text.Megaparsec
import Text.Megaparsec.Char

replParser :: Parser (ReplAction Annotation)
replParser =
  try helpParser
    <|> try infoParser
    <|> try bindParser
    <|> try bindTypeParser
    <|> try listBindingsParser
    <|> try treeParser
    <|> try watchParser
    <|> try evalParser
    <|> try versionsParser
    <|> outputJSParser

helpParser :: Parser (ReplAction Annotation)
helpParser = Help <$ string ":help"

infoParser :: Parser (ReplAction Annotation)
infoParser = do
  _ <- thenSpace (string ":info")
  Info <$> expressionParser

evalParser :: Parser (ReplAction Annotation)
evalParser =
  Evaluate
    <$> expressionParser

treeParser :: Parser (ReplAction Annotation)
treeParser = do
  _ <- thenSpace (string ":tree")
  Tree <$> expressionParser

bindParser :: Parser (ReplAction Annotation)
bindParser = do
  _ <- thenSpace (string ":bind")
  name <- thenSpace nameParser
  _ <- thenSpace (string "=")
  Bind name <$> expressionParser

bindTypeParser :: Parser (ReplAction Annotation)
bindTypeParser = do
  _ <- thenSpace (string ":bindType")
  BindType <$> typeDeclParser

listBindingsParser :: Parser (ReplAction Annotation)
listBindingsParser = ListBindings <$ string ":list"

watchParser :: Parser (ReplAction Annotation)
watchParser = do
  _ <- thenSpace (string ":watch")
  Watch <$> nameParser

versionsParser :: Parser (ReplAction Annotation)
versionsParser = do
  _ <- thenSpace (string ":versions")
  Versions <$> nameParser

outputJSParser :: Parser (ReplAction Annotation)
outputJSParser = do
  _ <- thenSpace (string ":outputJS")
  OutputJS <$> expressionParser
