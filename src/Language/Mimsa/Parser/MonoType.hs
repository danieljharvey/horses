{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.MonoType where

import qualified Data.Char as Char
import Data.Functor (($>))
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifiers (nameParser)
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Text.Megaparsec
import Text.Megaparsec.Char

monoTypeParser :: Parser MonoType
monoTypeParser =
  try functionParser
    <|> try simpleTypeParser

-- all the types except functions
simpleTypeParser :: Parser MonoType
simpleTypeParser =
  let parsers =
        try pairParser
          <|> try varParser
          <|> primitiveParser
   in orInBrackets parsers

primitiveParser :: Parser MonoType
primitiveParser = MTPrim mempty <$> primParser
  where
    primParser =
      try (string "String" $> MTString)
        <|> try (string "Boolean" $> MTBool)
        <|> try (string "Int" $> MTInt)
        <|> try (string "Unit" $> MTUnit)

functionParser :: Parser MonoType
functionParser = do
  one <- thenSpace simpleTypeParser -- let's not have more functions here for now to stop looping
  _ <- thenSpace (string "->")
  MTFunction mempty one <$> monoTypeParser

pairParser :: Parser MonoType
pairParser = do
  _ <- string "("
  one <- monoTypeParser
  _ <- thenSpace ","
  two <- monoTypeParser
  _ <- string ")"
  pure (MTPair mempty one two)

identifier :: Parser Text
identifier = takeWhile1P (Just "variable name") Char.isAlphaNum

varParser :: Parser MonoType
varParser = do
  MTVar mempty <$> (NamedVar <$> nameParser)
