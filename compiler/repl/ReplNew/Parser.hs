{-# LANGUAGE OverloadedStrings #-}

module ReplNew.Parser
  ( replParser,
  )
where

import Language.Mimsa.Parser
import Language.Mimsa.Types.AST
import ReplNew.Types
import Text.Megaparsec
import Text.Megaparsec.Char

type ReplActionAnn = ReplAction Annotation

replParser :: Parser ReplActionAnn
replParser =
  try helpParser
    <|> try listModulesParser
    <|> evalParser

helpParser :: Parser ReplActionAnn
helpParser = Help <$ string ":help"

evalParser :: Parser ReplActionAnn
evalParser =
  Evaluate
    <$> expressionParser

listModulesParser :: Parser ReplActionAnn
listModulesParser = ListModules <$ string ":modules"
