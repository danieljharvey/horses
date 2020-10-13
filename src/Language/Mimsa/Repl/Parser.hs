{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Parser
  ( replParser,
  )
where

import Language.Mimsa.Parser
import Language.Mimsa.Repl.Types
import Text.Megaparsec
import Text.Megaparsec.Char

replParser :: (Monoid ann) => Parser (ReplAction ann)
replParser =
  try helpParser
    <|> try infoParser
    <|> try bindParser
    <|> try bindTypeParser
    <|> try listBindingsParser
    <|> try treeParser
    <|> try watchParser
    <|> try evalParser
    <|> try tuiParser
    <|> try versionsParser
    <|> outputJSParser

helpParser :: Parser (ReplAction ann)
helpParser = Help <$ string ":help"

infoParser :: (Monoid ann) => Parser (ReplAction ann)
infoParser = do
  _ <- thenSpace (string ":info")
  Info <$> expressionParser

evalParser :: (Monoid ann) => Parser (ReplAction ann)
evalParser =
  Evaluate
    <$> expressionParser

treeParser :: (Monoid ann) => Parser (ReplAction ann)
treeParser = do
  _ <- thenSpace (string ":tree")
  Tree <$> expressionParser

bindParser :: (Monoid ann) => Parser (ReplAction ann)
bindParser = do
  _ <- thenSpace (string ":bind")
  name <- thenSpace nameParser
  _ <- thenSpace (string "=")
  Bind name <$> expressionParser

bindTypeParser :: Parser (ReplAction ann)
bindTypeParser = do
  _ <- thenSpace (string ":bindType")
  BindType <$> typeDeclParser

listBindingsParser :: Parser (ReplAction ann)
listBindingsParser = ListBindings <$ string ":list"

watchParser :: Parser (ReplAction ann)
watchParser = do
  _ <- thenSpace (string ":watch")
  Watch <$> nameParser

tuiParser :: Parser (ReplAction ann)
tuiParser = Tui <$ string ":tui"

versionsParser :: Parser (ReplAction ann)
versionsParser = do
  _ <- thenSpace (string ":versions")
  Versions <$> nameParser

outputJSParser :: (Monoid ann) => Parser (ReplAction ann)
outputJSParser = do
  _ <- thenSpace (string ":outputJS")
  OutputJS <$> expressionParser
