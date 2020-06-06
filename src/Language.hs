{-# LANGUAGE OverloadedStrings #-}

module Language
  ( parseExpr,
  )
where

import Control.Applicative ((<|>))
import Data.Functor
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Parser (Parser)
import qualified Parser as P
import Types (Expr (..), Name (..))

-- parse expr, using it all up
parseExpr :: Text -> Either Text Expr
parseExpr input = P.runParser expressionParser input
  >>= \(leftover, a) ->
    if T.length leftover == 0
      then Right a
      else Left ("Leftover input: " <> leftover)

expressionParser :: Parser Expr
expressionParser = boolParser <|> intParser <|> stringParser <|> letParser <|> varParser

protectedNames :: Set Name
protectedNames = S.fromList [(Name "let"), (Name "in")]

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

-----

varParser :: Parser Expr
varParser = MyVar <$> nameParser

nameParser :: Parser Name
nameParser =
  P.predicate
    (Name <$> P.identifier)
    (\name -> not $ S.member name protectedNames)

-----

letParser :: Parser Expr
letParser = MyLet <$> binderParser <*> equalsParser <*> inParser

binderParser :: Parser Name
binderParser = P.right (P.thenSpace (P.literal "let")) (P.thenSpace nameParser)

equalsParser :: Parser Expr
equalsParser =
  P.right (P.thenSpace (P.literal "=")) (P.thenSpace expressionParser)

inParser :: Parser Expr
inParser = P.right (P.thenSpace (P.literal "in")) expressionParser
