{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Helpers
  ( parseAndFormat,
    thenSpace,
    between2,
    addLocation,
    withLocation,
    maybePred,
    filterProjectedNames,
    filterProtectedOperators,
    inBrackets,
    orInBrackets,
    literalWithSpace,
    withOptionalSpace,
    chainl1,
    spaceThen,
    optionalSpaceThen,
  )
where

import Control.Monad (void)
import Data.Bifunctor (first)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.ExprUtils
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Text.Megaparsec
import Text.Megaparsec.Char

-- run a parser and then run Megaparsec pretty printer on the error
parseAndFormat :: Parser a -> Text -> Either Text a
parseAndFormat p = first (T.pack . errorBundlePretty) . parse (p <* eof) "repl"

-- parses between two chars
between2 :: Char -> Char -> Parser a -> Parser a
between2 a b parser = do
  _ <- char a
  val <- parser
  _ <- char b
  pure val

-----

-- helper for adding location to a parser
withLocation :: (Annotation -> a -> b) -> Parser a -> Parser b
withLocation withP p = do
  start <- getOffset
  value <- p
  end <- getOffset
  pure (withP (Location start end) value)

-- | wraps any parser of Exprs and adds location information
addLocation :: Parser ParserExpr -> Parser ParserExpr
addLocation = withLocation (mapOuterExprAnnotation . const)

-----

inBrackets :: Parser a -> Parser a
inBrackets = between2 '(' ')'

-----

orInBrackets :: Parser a -> Parser a
orInBrackets parser = try parser <|> try (inBrackets parser)

-----

maybePred :: (Show a) => Parser a -> (a -> Maybe b) -> Parser b
maybePred parser predicate' = try $ do
  a <- parser
  case predicate' a of
    Just b -> pure b
    _ -> fail $ T.unpack $ "Predicate did not hold for " <> T.pack (show a)

-----

filterProjectedNames :: Text -> Maybe Text
filterProjectedNames tx =
  if S.member tx protectedNames
    then Nothing
    else Just tx

filterProtectedOperators :: Text -> Maybe Text
filterProtectedOperators tx =
  if S.member tx protectedOperators
    then Nothing
    else Just tx

---

literalWithSpace :: Text -> Parser ()
literalWithSpace tx = void $ withOptionalSpace (string tx)

withOptionalSpace :: Parser a -> Parser a
withOptionalSpace p = do
  _ <- space
  a <- p
  _ <- space
  pure a

-- looks for Parser a followed by 1 or more spaces
thenSpace :: Parser a -> Parser a
thenSpace parser = do
  _ <- space
  val <- parser
  _ <- space1
  pure val

-- at least one space then Parser a
spaceThen :: Parser a -> Parser a
spaceThen p = do
  _ <- space1
  p

-- at least one space then Parser a
optionalSpaceThen :: Parser a -> Parser a
optionalSpaceThen p = do
  _ <- space
  p

-----

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
