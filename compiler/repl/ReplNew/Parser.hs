{-# LANGUAGE OverloadedStrings #-}

module ReplNew.Parser
  ( replParser,
  )
where
import Data.Functor
import Language.Mimsa.Backend.Types

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Language.Mimsa.Parser
import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Types.AST
import ReplNew.Types
import Text.Megaparsec

type ReplActionAnn = ReplAction Annotation

replParser :: Parser ReplActionAnn
replParser =
  try helpParser
    <|> listModulesParser
    <|> listBindingsParser
    <|> try addBindingParser
    <|> try outputJSModuleParser
    <|> evalParser

helpParser :: Parser ReplActionAnn
helpParser = Help <$ myString ":help"

evalParser :: Parser ReplActionAnn
evalParser =
  Evaluate
    <$> expressionParser

listModulesParser :: Parser ReplActionAnn
listModulesParser = do
  myString ":modules"
  modName <- optional moduleNameParser
  pure (ListModules modName)

listBindingsParser :: Parser ReplActionAnn
listBindingsParser = ListBindings <$ myString ":list"

-- return very basic error
explode :: String -> Parser any
explode msg =
  failure Nothing (S.singleton (Label (NE.fromList msg)))

addBindingParser :: Parser ReplActionAnn
addBindingParser = AddBinding <$> singleModuleItemParser
  where
    singleModuleItemParser = do
      _ <- myString ":bind"
      item <- moduleParser
      case item of
        [] -> explode "Expected a module binding"
        [a] -> pure a
        _other -> explode "Expected a single module binding"

backendParser :: Parser (Maybe Backend)
backendParser =
  myString "javascript" $> Just ESModulesJS
    <|> myString "typescript" $> Just Typescript
    <|> pure Nothing

outputJSModuleParser :: Parser ReplActionAnn
outputJSModuleParser = do
  myString ":compile"
  be <- backendParser
  OutputModuleJS be <$> moduleNameParser

