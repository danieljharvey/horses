{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Type (typeParser) where

import Data.Functor (($>))
import Calc.Parser.Shared
  (




    stringLiteral,
  addTypeLocation, myLexeme
  )
import Calc.Types.Type
import Text.Megaparsec
  ( MonadParsec (try),
    (<|>),
  )
import Calc.Parser.Types

-- | top-level parser for type signatures
typeParser :: Parser ParserType
typeParser =
  myLexeme (addTypeLocation tyPrimitiveParser)

tyPrimitiveParser :: Parser ParserType
tyPrimitiveParser = TPrim mempty <$> tyPrimParser
  where
    tyPrimParser =
      try (stringLiteral "Boolean" $> TBool)
        <|> try (stringLiteral "Integer" $> TInt)
