{-# LANGUAGE OverloadedStrings #-}

module Parser where

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

predicate :: Parser a -> (a -> Bool) -> Parser a
predicate parser predicate' =
  mkParser
    ( \input ->
        runParser parser input
          >>= \(next, a) ->
            if predicate' a
              then Right (next, a)
              else Left "Predicate did not hold"
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

-- parser with at least one space after
thenSpace :: Parser a -> Parser a
thenSpace parser = right space0 (left parser space1)
{-

interface Element {
  name: string;
  attributes: [string, string][];
  children: Element[];
}

export const quotedString: Parser<string> = stringBetween('"', '"');

export const attributePair = pair(
  identifier,
  right(matchLiteral("="), quotedString),
);

export const attributes = zeroOrMore(right(space1, attributePair));

const elementStart: Parser<[string, [string, string][]]> = right(
  matchLiteral("<"),
  pair(identifier, attributes),
);

export const singleElement: Parser<Element> = map(
  left(elementStart, matchLiteral("/>")),
  ([name, attributes]) => ({
    name,
    attributes,
    children: [],
  }),
);

const openElement = map(
  left(elementStart, matchLiteral(">")),
  ([name, attributes]) => ({
    name,
    attributes,
    children: [],
  }),
);

const closeElement = (expectedName: string) =>
  pred(
    right(matchLiteral("</"), left(identifier, matchLiteral(">"))),
    (name) => name === expectedName,
  );

const parentElement = andThen(
  openElement,
  (el) =>
    map(left(zeroOrMore(element()), closeElement(el.name)), (children) => ({
      ...el,
      children,
    })),
);

export const element = (): Parser<Element> =>
  either(singleElement, parentElement);

export const runParser = <A>(
  parser: Parser<A>,
  input: string,
): R.Result<Error, [string, A]> => parser.parse(input);

const splitString = (
  input: string,
  length: number,
): [string | null, string] => {
  const match = input.slice(0, length);
  const actualMatch = match.length >= length ? match : null;
  const rest = input.slice(length);
  return [actualMatch, rest];
};
-}
