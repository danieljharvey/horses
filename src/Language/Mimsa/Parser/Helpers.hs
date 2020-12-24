{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Helpers where

import Data.Bifunctor (first)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Text.Megaparsec
import Text.Megaparsec.Char

-- run a parser and then run Megaparsec pretty printer on the error
parseAndFormat :: Parser a -> Text -> Either Text a
parseAndFormat p = first (T.pack . errorBundlePretty) . parse (p <* eof) "repl"

-- looks for Parser a followed by 1 or more spaces
thenSpace :: Parser a -> Parser a
thenSpace parser = do
  _ <- space
  val <- parser
  _ <- space1
  pure val

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

inProtected :: Text -> Maybe Text
inProtected tx =
  if S.member tx protectedNames
    then Nothing
    else Just tx

---

literalWithSpace :: Text -> Parser ()
literalWithSpace tx = () <$ withOptionalSpace (string tx)

withOptionalSpace :: Parser a -> Parser a
withOptionalSpace p = do
  _ <- space
  a <- p
  _ <- space
  pure a
-----
