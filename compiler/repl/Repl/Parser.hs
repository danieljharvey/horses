{-# LANGUAGE OverloadedStrings #-}

module Repl.Parser
  ( replParser,
  )
where

import Data.Functor (($>))
import Language.Mimsa.Backend.Types
import Language.Mimsa.Parser
import Language.Mimsa.Types.AST
import Repl.Types
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
    <|> try evalParser
    <|> try outputJSParser
    <|> try typeSearchParser

helpParser :: Parser ReplActionAnn
helpParser = Help <$ string ":help"

infoParser :: Parser ReplActionAnn
infoParser = do
  myString ":info"
  Info <$> expressionParser

evalParser :: Parser ReplActionAnn
evalParser =
  Evaluate
    <$> expressionParser

bindParser :: Parser ReplActionAnn
bindParser = do
  myString ":bind"
  name <- nameParser
  myString "="
  Bind name <$> expressionParser

bindTypeParser :: Parser ReplActionAnn
bindTypeParser = do
  myString ":bindType"
  BindType <$> typeDeclParser

listBindingsParser :: Parser ReplActionAnn
listBindingsParser = ListBindings <$ string ":list"

backendParser :: Parser (Maybe Backend)
backendParser =
  myString "javascript" $> Just ESModulesJS
    <|> myString "typescript" $> Just Typescript
    <|> pure Nothing

outputJSParser :: Parser ReplActionAnn
outputJSParser = do
  myString ":outputJS"
  be <- backendParser
  OutputJS be <$> expressionParser

typeSearchParser :: Parser ReplActionAnn
typeSearchParser = do
  myString ":search"
  TypeSearch <$> monoTypeParser


