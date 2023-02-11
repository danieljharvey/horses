{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Primitives
  ( primParser,
    intParser,
  )
where

import Calc.Parser.Shared
import Calc.Parser.Types
import Calc.Types.Expr
import Calc.Types.Prim
import Control.Applicative
import Data.Functor (($>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

----

intParser :: Parser Integer
intParser =
  L.signed (string "" $> ()) L.decimal

---

truePrimParser :: Parser Prim
truePrimParser = PBool <$> trueParser

trueParser :: Parser Bool
trueParser = stringLiteral "True" $> True

falsePrimParser :: Parser Prim
falsePrimParser = PBool <$> falseParser

falseParser :: Parser Bool
falseParser = stringLiteral "False" $> False

---

primParser :: Parser ParserExpr
primParser =
  myLexeme $
    addLocation $
      EPrim mempty . PInt <$> intParser
        <|> EPrim mempty <$> truePrimParser
        <|> EPrim mempty <$> falsePrimParser
