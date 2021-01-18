{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Parser
  ( replParser,
  )
where

import qualified Data.Text as T
import Language.Mimsa.Parser
import Language.Mimsa.Parser.Literal
import Language.Mimsa.Repl.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project
import Text.Megaparsec
import Text.Megaparsec.Char

type ReplActionAnn = ReplAction Annotation

replParser :: Parser ReplActionAnn
replParser =
  try helpParser
    <|> try infoParser
    <|> try bindParser
    <|> try bindTypeParser
    <|> try listBindingsParser
    <|> try treeParser
    <|> try evalParser
    <|> try versionsParser
    <|> try outputJSParser
    <|> try typeSearchParser
    <|> try addUnitTestParser

helpParser :: Parser ReplActionAnn
helpParser = Help <$ string ":help"

infoParser :: Parser ReplActionAnn
infoParser = do
  _ <- thenSpace (string ":info")
  Info <$> expressionParser

evalParser :: Parser ReplActionAnn
evalParser =
  Evaluate
    <$> expressionParser

treeParser :: Parser ReplActionAnn
treeParser = do
  _ <- thenSpace (string ":tree")
  Tree <$> expressionParser

bindParser :: Parser ReplActionAnn
bindParser = do
  _ <- thenSpace (string ":bind")
  name <- thenSpace nameParser
  _ <- thenSpace (string "=")
  Bind name <$> expressionParser

bindTypeParser :: Parser ReplActionAnn
bindTypeParser = do
  _ <- thenSpace (string ":bindType")
  BindType <$> typeDeclParser

listBindingsParser :: Parser ReplActionAnn
listBindingsParser = ListBindings <$ string ":list"

versionsParser :: Parser ReplActionAnn
versionsParser = do
  _ <- thenSpace (string ":versions")
  Versions <$> nameParser

outputJSParser :: Parser ReplActionAnn
outputJSParser = do
  _ <- thenSpace (string ":outputJS")
  OutputJS <$> expressionParser

typeSearchParser :: Parser ReplActionAnn
typeSearchParser = do
  _ <- thenSpace (string ":search")
  TypeSearch <$> monoTypeParser

addUnitTestParser :: Parser ReplActionAnn
addUnitTestParser = do
  _ <- thenSpace (string ":addUnitTest")
  str <- thenSpace stringLiteral
  AddUnitTest (TestName $ T.pack str) <$> expressionParser
