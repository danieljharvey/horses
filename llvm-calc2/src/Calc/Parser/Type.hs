{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Type (typeParser) where

import Calc.Parser.Shared
  ( addTypeLocation,
    myLexeme,
    stringLiteral,
  )
import Calc.Parser.Types
import Calc.Types.Type
import Data.Functor (($>))
import Text.Megaparsec
  ( MonadParsec (try),
    (<|>),
  )

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
