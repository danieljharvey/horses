{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser.Op (opParser) where

import Data.Functor (($>))
import Data.Text (Text)
import Data.Void
import Smol.Core.Parser.Shared
import Smol.Core.Types
import Text.Megaparsec

type Parser = Parsec Void Text

opParser :: Parser Op
opParser =
  {- try
    ( Custom <$> infixOpParser
    ) -}
  try
    ( myString "=="
        $> OpEquals
    ) {-
      <|> try
        ( myString "-"
            $> Subtract
        )
      <|> try
        ( myString "<>"
            $> ArrayConcat
        )
      <|> try
        ( myString ">="
            $> GreaterThanOrEqualTo
        )
      <|> try
        ( myString "<="
            $> LessThanOrEqualTo
        )
      <|> try
        ( myString ">"
            $> GreaterThan
        )
      <|> try
        ( myString "<"
            $> LessThan
        )
      <|> try
        ( myString "++"
            $> StringConcat
        )
        -}
    <|> try
      ( myString "+"
          $> OpAdd
      )
