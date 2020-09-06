{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Parser where

import Control.Applicative ((<|>))
import Language.Mimsa.Parser
import Language.Mimsa.Repl.Types

replParser :: Parser ReplAction
replParser =
  helpParser
    <|> infoParser
    <|> bindParser
    <|> listBindingsParser
    <|> treeParser
    <|> watchParser
    <|> evalParser
    <|> tuiParser
    <|> versionsParser

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

watchParser :: Parser ReplAction
watchParser = Watch <$ literal ":watch"

tuiParser :: Parser ReplAction
tuiParser = Tui <$ literal ":tui"

versionsParser :: Parser ReplAction
versionsParser = do
  _ <- thenSpace (literal ":versions")
  Versions <$> nameParser
