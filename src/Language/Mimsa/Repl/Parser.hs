{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Parser where

import Control.Applicative ((<|>))
import qualified Language.Mimsa.Language as Mimsa
import qualified Language.Mimsa.Parser as P
import Language.Mimsa.Repl.Types

replParser :: P.Parser ReplAction
replParser = evaluateParser <|> bindParser <|> listBindingsParser

evaluateParser :: P.Parser ReplAction
evaluateParser =
  Evaluate
    <$> P.right
      (P.thenSpace (P.literal ":info"))
      Mimsa.expressionParser

bindParser :: P.Parser ReplAction
bindParser =
  Bind
    <$> P.right (P.thenSpace (P.literal ":bind")) (P.thenSpace Mimsa.nameParser)
    <*> P.right (P.thenSpace (P.literal "=")) Mimsa.expressionParser

listBindingsParser :: P.Parser ReplAction
listBindingsParser = ListBindings <$ P.literal ":list"
