module Calc.Parser.Shared
  ( inBrackets,
    myLexeme,
    withLocation,
    stringLiteral,
    addLocation,
    addTypeLocation,
  )
where

import Calc.ExprUtils
import Calc.Parser.Types
import Calc.TypeUtils
import Calc.Types.Annotation
import Data.Functor (($>))
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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

-- | wraps any parser of Type and adds location information
addTypeLocation :: Parser ParserType -> Parser ParserType
addTypeLocation = withLocation (mapOuterTypeAnnotation . const)

inBrackets :: Parser a -> Parser a
inBrackets = between2 '(' ')'

myLexeme :: Parser a -> Parser a
myLexeme = L.lexeme (L.space space1 empty empty)

stringLiteral :: Text -> Parser ()
stringLiteral s = myLexeme (string s) $> ()
