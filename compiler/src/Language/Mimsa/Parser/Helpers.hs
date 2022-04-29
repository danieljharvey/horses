{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Helpers
  ( parseAndFormat,
    between2,
    addLocation,
    withLocation,
    maybePred,
    filterProjectedNames,
    filterProtectedOperators,
    inBrackets,
    orInBrackets,
    chainl1,
  )
where

import Data.Bifunctor (first)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Parser.Lexeme
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
  _ <- myLexeme (char a)
  val <- parser
  _ <- myLexeme (char b)
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

-- | modify the outer annotation of an expression
-- useful for adding line numbers during parsing
mapOuterExprAnnotation :: (ann -> ann) -> Expr a ann -> Expr a ann
mapOuterExprAnnotation f expr' =
  case expr' of
    MyInfix ann a op b -> MyInfix (f ann) a op b
    MyAnnotation ann expr mt -> MyAnnotation (f ann) expr mt
    MyLiteral ann a -> MyLiteral (f ann) a
    MyVar ann a -> MyVar (f ann) a
    MyLet ann a b c -> MyLet (f ann) a b c
    MyLetPattern ann a b c -> MyLetPattern (f ann) a b c
    MyLambda ann a b -> MyLambda (f ann) a b
    MyApp ann a b -> MyApp (f ann) a b
    MyIf ann a b c -> MyIf (f ann) a b c
    MyPair ann a b -> MyPair (f ann) a b
    MyRecord ann as -> MyRecord (f ann) as
    MyRecordAccess ann a b -> MyRecordAccess (f ann) a b
    MyArray ann as -> MyArray (f ann) as
    MyData ann a b -> MyData (f ann) a b
    MyConstructor ann a -> MyConstructor (f ann) a
    MyTypedHole ann a -> MyTypedHole (f ann) a
    MyDefineInfix ann a b c -> MyDefineInfix (f ann) a b c
    MyPatternMatch ann a b -> MyPatternMatch (f ann) a b

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
