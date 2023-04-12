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
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec
  ( MonadParsec (try),
    label,
    sepBy1,
    (<|>),
  )

-- | top-level parser for type signatures
typeParser :: Parser ParserType
typeParser =
  tyPrimitiveParser <|> tyTupleParser

tyPrimitiveParser :: Parser ParserType
tyPrimitiveParser = myLexeme $ addTypeLocation $ TPrim mempty <$> tyPrimParser
  where
    tyPrimParser =
      try (stringLiteral "Boolean" $> TBool)
        <|> try (stringLiteral "Integer" $> TInt)

tyTupleParser :: Parser ParserType
tyTupleParser = label "tuple" $
  addTypeLocation $ do
    _ <- stringLiteral "("
    neArgs <- NE.fromList <$> sepBy1 typeParser (stringLiteral ",")
    neTail <- case NE.nonEmpty (NE.tail neArgs) of
      Just ne -> pure ne
      _ -> fail "Expected at least two items in a tuple"
    _ <- stringLiteral ")"
    pure (TTuple mempty (NE.head neArgs) neTail)
