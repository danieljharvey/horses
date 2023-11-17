{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser.Shared
  ( emptyParseDep,
    inBrackets,
    orInBrackets,
    inCurlyBrackets,
    myLexeme,
    withLocation,
    myString,
    addLocation,
    mapOuterExprAnnotation,
    maybePred,
    chainl1,
    commaSep,
  )
where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Smol.Core.ExprUtils
import Smol.Core.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

type ParserExpr = Expr ParseDep Annotation

between2 :: Char -> Char -> Parser a -> Parser a
between2 a b parser = do
  _ <- myLexeme (char a)
  val <- parser
  _ <- myLexeme (char b)
  pure val

withLocation :: (Annotation -> a -> b) -> Parser a -> Parser b
withLocation withP p = do
  start <- getOffset
  value <- p
  end <- getOffset
  pure (withP (Location start end) value)

-- | wraps any parser of Exprs and adds location information
addLocation :: Parser ParserExpr -> Parser ParserExpr
addLocation = withLocation (mapOuterExprAnnotation . const)

inBrackets :: Parser a -> Parser a
inBrackets = between2 '(' ')'

orInBrackets :: Parser a -> Parser a
orInBrackets parser = try parser <|> inBrackets parser

inCurlyBrackets :: Parser a -> Parser a
inCurlyBrackets = between2 '{' '}'

maybePred :: (Show a) => Parser a -> (a -> Maybe b) -> Parser b
maybePred parser predicate' = try $ do
  a <- parser
  case predicate' a of
    Just b -> pure b
    _ -> fail $ T.unpack $ "Predicate did not hold for " <> T.pack (show a)

-- | stolen from Parsec, allows parsing infix expressions without recursion
-- death
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do x <- p; rest x
  where
    rest x =
      do
        f <- op
        y <- p
        rest (f x y)
        <|> return x

myLexeme :: Parser a -> Parser a
myLexeme =
  L.lexeme
    ( L.space
        space1
        (L.skipLineComment "//")
        (L.skipBlockComment "/*" "*/")
    )

myString :: Text -> Parser ()
myString s = myLexeme (string s) $> ()

commaSep :: Parser p -> Parser (NE.NonEmpty p)
commaSep p = NE.fromList <$> p `sepBy1` myString ","
