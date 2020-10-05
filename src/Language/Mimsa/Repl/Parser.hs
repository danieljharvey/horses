{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Parser
  ( replParser,
  )
where

import Control.Applicative ((<|>))
import Language.Mimsa.Parser
import Language.Mimsa.Repl.Types

replParser :: (Monoid ann) => Parser (ReplAction ann)
replParser =
  helpParser
    <|> infoParser
    <|> bindParser
    <|> bindTypeParser
    <|> listBindingsParser
    <|> treeParser
    <|> watchParser
    <|> evalParser
    <|> tuiParser
    <|> versionsParser
    <|> outputJSParser
    <|> failer

failer :: Parser a
failer = parseFail (\input -> "Could not parse expression for >>>" <> input <> "<<<")

helpParser :: Parser (ReplAction ann)
helpParser = Help <$ literal ":help"

infoParser :: (Monoid ann) => Parser (ReplAction ann)
infoParser =
  Info
    <$> right
      (thenSpace (literal ":info"))
      expressionParser

evalParser :: (Monoid ann) => Parser (ReplAction ann)
evalParser =
  Evaluate
    <$> expressionParser

treeParser :: (Monoid ann) => Parser (ReplAction ann)
treeParser = Tree <$> right (thenSpace (literal ":tree")) expressionParser

bindParser :: (Monoid ann) => Parser (ReplAction ann)
bindParser =
  Bind
    <$> right (thenSpace (literal ":bind")) (thenSpace nameParser)
    <*> right (thenSpace (literal "=")) expressionParser

bindTypeParser :: Parser (ReplAction ann)
bindTypeParser = do
  _ <- thenSpace (literal ":bindType")
  BindType <$> typeDeclParser

listBindingsParser :: Parser (ReplAction ann)
listBindingsParser = ListBindings <$ literal ":list"

watchParser :: Parser (ReplAction ann)
watchParser = do
  _ <- thenSpace (literal ":watch")
  Watch <$> nameParser

tuiParser :: Parser (ReplAction ann)
tuiParser = Tui <$ literal ":tui"

versionsParser :: Parser (ReplAction ann)
versionsParser = do
  _ <- thenSpace (literal ":versions")
  Versions <$> nameParser

outputJSParser :: (Monoid ann) => Parser (ReplAction ann)
outputJSParser = do
  _ <- thenSpace (literal ":outputJS")
  OutputJS <$> expressionParser
