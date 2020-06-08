{-# LANGUAGE OverloadedStrings #-}

module Language
  ( parseExpr,
    parseExpr',
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

parseExpr' :: Text -> Either Text Expr
parseExpr' input = snd <$> P.runParser expressionParser input

expressionParser :: Parser Expr
expressionParser =
  literalParser
    <|> letParser
    <|> ifParser
    <|> functionParser

literalParser :: Parser Expr
literalParser =
  boolParser
    <|> intParser
    <|> stringParser

functionParser :: Parser Expr
functionParser =
  lambdaParser
    <|> appParser
    <|> varParser

protectedNames :: Set Name
protectedNames =
  S.fromList
    [ Name "let",
      Name "in",
      Name "if",
      Name "then",
      Name "else"
    ]

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

-----

lambdaParser :: Parser Expr
lambdaParser = MyLambda <$> slashNameBinder <*> arrowExprBinder

-- matches \varName
slashNameBinder :: Parser Name
slashNameBinder = P.right (P.literal "\\") (P.thenSpace nameParser)

arrowExprBinder :: Parser Expr
arrowExprBinder = P.right (P.thenSpace (P.literal "->")) expressionParser

-----

appParser :: Parser Expr
appParser = MyApp <$> funcParser <*> argParser

funcParser :: Parser Expr
funcParser = P.thenSpace (varParser <|> literalParser)

argParser :: Parser Expr
argParser = expressionParser

-----

ifParser :: Parser Expr
ifParser = MyIf <$> predParser <*> thenParser <*> elseParser

predParser :: Parser Expr
predParser = P.right (P.thenSpace (P.literal "if")) expressionParser

thenParser :: Parser Expr
thenParser = P.right (P.thenSpace (P.literal "then")) expressionParser

elseParser :: Parser Expr
elseParser = P.right (P.thenSpace (P.literal "else")) expressionParser
