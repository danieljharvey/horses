{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Parser
  ( replParser,
  )
where

import Control.Applicative ((<|>))
import Language.Mimsa.Parser
import Language.Mimsa.Repl.Types

type ReplAction' = ReplAction ()

replParser :: Parser ReplAction'
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

helpParser :: Parser ReplAction'
helpParser = Help <$ literal ":help"

infoParser :: Parser ReplAction'
infoParser =
  Info
    <$> right
      (thenSpace (literal ":info"))
      expressionParser

evalParser :: Parser ReplAction'
evalParser =
  Evaluate
    <$> expressionParser

treeParser :: Parser ReplAction'
treeParser = Tree <$> right (thenSpace (literal ":tree")) expressionParser

bindParser :: Parser ReplAction'
bindParser =
  Bind
    <$> right (thenSpace (literal ":bind")) (thenSpace nameParser)
    <*> right (thenSpace (literal "=")) expressionParser

bindTypeParser :: Parser ReplAction'
bindTypeParser = do
  _ <- thenSpace (literal ":bindType")
  BindType <$> typeDeclParser

listBindingsParser :: Parser ReplAction'
listBindingsParser = ListBindings <$ literal ":list"

watchParser :: Parser ReplAction'
watchParser = do
  _ <- thenSpace (literal ":watch")
  Watch <$> nameParser

tuiParser :: Parser ReplAction'
tuiParser = Tui <$ literal ":tui"

versionsParser :: Parser ReplAction'
versionsParser = do
  _ <- thenSpace (literal ":versions")
  Versions <$> nameParser

outputJSParser :: Parser ReplAction'
outputJSParser = do
  _ <- thenSpace (literal ":outputJS")
  OutputJS <$> expressionParser
