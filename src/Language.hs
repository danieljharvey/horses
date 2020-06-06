{-# LANGUAGE OverloadedStrings #-}

module Language
  ( parseExpr,
  )
where

import Control.Applicative ((<|>))
import Data.Functor
import Data.Text (Text)
import Parser (Parser)
import qualified Parser as P
import Types (Expr (..))

parseExpr :: Text -> Either Text Expr
parseExpr input = snd <$> P.runParser expressionParser input

expressionParser :: Parser Expr
expressionParser = boolParser <|> intParser <|> stringParser

----

boolParser :: Parser Expr
boolParser = trueParser <|> falseParser

trueParser :: Parser Expr
trueParser = P.literal "True" $> MyBool True

falseParser :: Parser Expr
falseParser = P.literal "False" $> MyBool False

-----

intParser :: Parser Expr
intParser = MyInt <$> P.integer

-----

stringParser :: Parser Expr
stringParser = MyString <$> (P.between '"')
