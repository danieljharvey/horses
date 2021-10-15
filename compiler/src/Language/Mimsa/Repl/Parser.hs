{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Parser
  ( replParser,
  )
where

import Data.Functor (($>))
import Language.Mimsa.Backend.Types
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
    <|> try graphParser
    <|> try projectGraphParser
    <|> try evalParser
    <|> try versionsParser
    <|> try outputJSParser
    <|> try typeSearchParser
    <|> try addUnitTestParser
    <|> listTestsParser

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

graphParser :: Parser ReplActionAnn
graphParser = do
  _ <- thenSpace (string ":graph")
  Graph <$> expressionParser

projectGraphParser :: Parser ReplActionAnn
projectGraphParser = ProjectGraph <$ string ":projectGraph"

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

backendParser :: Parser (Maybe Backend)
backendParser =
  thenSpace (string "commonjs") $> Just CommonJS
    <|> thenSpace (string "es-modules") $> Just ESModulesJS
    <|> thenSpace (string "typescript") $> Just Typescript
    <|> pure Nothing

outputJSParser :: Parser ReplActionAnn
outputJSParser = do
  _ <- thenSpace (string ":outputJS")
  be <- backendParser
  OutputJS be <$> expressionParser

typeSearchParser :: Parser ReplActionAnn
typeSearchParser = do
  _ <- thenSpace (string ":search")
  TypeSearch <$> monoTypeParser

addUnitTestParser :: Parser ReplActionAnn
addUnitTestParser = do
  _ <- thenSpace (string ":addTest")
  (MyString (StringType str)) <- thenSpace stringLiteral
  AddUnitTest (TestName str) <$> expressionParser

listTestsParser :: Parser ReplActionAnn
listTestsParser = do
  _ <- string ":tests"
  maybeName <-
    optional
      ( do
          _ <-
            space1
          nameParser
      )
  pure $ ListTests maybeName
