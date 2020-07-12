{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Syntax.Parser where

import Control.Applicative
import qualified Data.Char as Char
import Data.List.NonEmpty
import Data.Text (Text)
import qualified Data.Text as T

type Error = Text

newtype Parser a = Parser (Text -> Either Error (Text, a))

mkParser :: (Text -> Either Error (Text, a)) -> Parser a
mkParser = Parser

runParser :: Parser a -> Text -> Either Error (Text, a)
runParser (Parser parser) = parser

runParserComplete :: Parser a -> Text -> Either Error a
runParserComplete parser input = runParser parser input
  >>= \(leftover, a) ->
    if T.length leftover == 0
      then Right a
      else Left ("Leftover input: >>" <> leftover <> "<<")

instance Functor Parser where
  fmap f (Parser parser) =
    mkParser
      ( \input -> case parser input of
          Left e -> Left e
          Right (rest, a) -> Right (rest, f (a))
      )

instance Applicative Parser where
  pure a = mkParser (\input -> Right (input, a))

  parserF <*> parserA =
    mkParser
      ( \input -> runParser parserF input
          >>= \(rest, f) -> runParser (fmap f parserA) rest
      )

instance Alternative Parser where
  empty = mkParser (const $ Left "Failed")

  parserA <|> parserB =
    mkParser
      ( \input -> case runParser parserA input of
          Right a -> Right a
          Left _ -> runParser parserB input
      )

instance Monad Parser where
  return = pure

  parserA >>= aToParserB =
    mkParser
      ( \input ->
          runParser parserA input
            >>= \(next, a) -> runParser (aToParserB a) next
      )

pair :: Parser a -> Parser b -> Parser (a, b)
pair parserA parserB =
  mkParser
    ( \input ->
        runParser parserA input
          >>= \(restA, a) -> runParser parserB restA
            >>= \(restB, b) -> pure (restB, (a, b))
    )

left :: Parser a -> Parser b -> Parser a
left parserA parserB = fmap fst (pair parserA parserB)

right :: Parser a -> Parser b -> Parser b
right parserA parserB = fmap snd (pair parserA parserB)

oneOrMore :: Parser a -> Parser (NonEmpty a)
oneOrMore parser = fmap fromList (some parser)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many

char :: Parser Char
char =
  mkParser
    ( \input ->
        if T.length input == 0
          then Left "Empty string"
          else Right (T.tail input, T.head input)
    )

predicate :: (Show a) => Parser a -> (a -> Bool) -> Parser a
predicate parser predicate' =
  mkParser
    ( \input ->
        runParser parser input
          >>= \(next, a) ->
            if predicate' a
              then Right (next, a)
              else Left $ "Predicate did not hold for " <> T.pack (show a)
    )

literal :: Text -> Parser Text
literal lit =
  mkParser
    ( \input ->
        if T.take (T.length lit) input == lit
          then Right (T.drop (T.length lit) input, lit)
          else Left $ "Could not match literal " <> lit
    )

alphaNumeric :: Parser Char
alphaNumeric = predicate char Char.isAlphaNum

numeric :: Parser Int
numeric = fmap Char.digitToInt (predicate char Char.isDigit)

natural :: Parser Int
natural = fmap (foldl (\as a -> a + (as * 10)) 0) (oneOrMore numeric)

integer :: Parser Int
integer =
  fmap (\a -> a * (-1)) (right (literal "-") natural) -- "-3"
    <|> right (literal "+") natural -- "+3"
    <|> natural -- "3"

identifier :: Parser Text
identifier = fmap (foldr T.cons "") (oneOrMore alphaNumeric)

whitespace :: Parser Char
whitespace = predicate char Char.isSpace

space0 :: Parser [Char]
space0 = zeroOrMore whitespace

space1 :: Parser (NonEmpty Char)
space1 = oneOrMore whitespace

newline :: Parser Char
newline = predicate char (\a -> a == '\n')

between :: Char -> Parser Text
between char' =
  fmap
    (foldr T.cons "")
    ( right
        (literal (T.singleton char'))
        ( left
            (zeroOrMore (predicate char (\c -> c /= char')))
            (literal (T.singleton char'))
        )
    )

between2 :: Char -> Char -> Parser a -> Parser a
between2 char1 char2 parser =
  right
    (literal (T.singleton char1))
    ( left
        parser
        (literal (T.singleton char2))
    )

-- parser with at least one space after
thenSpace :: Parser a -> Parser a
thenSpace parser = right space0 (left parser space1)

thenOptionalSpace :: Parser a -> Parser a
thenOptionalSpace parser = left parser space0

-- recurse, but only once
leftRec ::
  Parser a ->
  Parser (a -> a) ->
  Parser a
leftRec p op = rest =<< p
  where
    rest x =
      do
        f <- op
        rest (f x)
        <|> return x
