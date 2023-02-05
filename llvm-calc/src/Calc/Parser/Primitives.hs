{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Primitives
  ( primParser,
    intParser,
  )
where

import Calc.Parser.Shared
import Calc.Parser.Types
import Calc.Types.Expr
import Data.Functor (($>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

----

intParser :: Parser Int
intParser =
  L.signed (string "" $> ()) L.decimal

primParser :: Parser ParserExpr
primParser = myLexeme $ addLocation (EPrim mempty <$> intParser)
