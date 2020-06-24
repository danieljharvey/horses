{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Syntax.Language
  ( parseExpr,
    parseExpr',
    expressionParser,
    nameParser,
  )
where

import Control.Applicative ((<|>))
import Data.Functor
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Syntax.Parser (Parser)
import qualified Language.Mimsa.Syntax.Parser as P
import Language.Mimsa.Types
  ( Expr (..),
    Literal (..),
    Name,
    StringType (..),
    SumSide (..),
    mkName,
    validName,
  )

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
    <|> varParser
    <|> complexParser

literalParser :: Parser Expr
literalParser =
  boolParser
    <|> unitParser
    <|> intParser
    <|> stringParser

complexParser :: Parser Expr
complexParser =
  let parsers =
        ( letPairParser
            <|> letParser
            <|> ifParser
            <|> lambdaParser
            <|> pairParser
            <|> sumParser
        )
   in (P.between2 '(' ')' (parsers <|> appParser)) <|> parsers

protectedNames :: Set Text
protectedNames =
  S.fromList
    [ "let",
      "in",
      "if",
      "then",
      "else",
      "True",
      "False",
      "Unit",
      "Left",
      "Right"
    ]

----

boolParser :: Parser Expr
boolParser = trueParser <|> falseParser

trueParser :: Parser Expr
trueParser = P.literal "True" $> MyLiteral (MyBool True)

falseParser :: Parser Expr
falseParser = P.literal "False" $> MyLiteral (MyBool False)

-----

unitParser :: Parser Expr
unitParser = P.literal "Unit" $> MyLiteral MyUnit

-----

intParser :: Parser Expr
intParser = MyLiteral <$> MyInt <$> P.integer

-----

stringParser :: Parser Expr
stringParser = (MyLiteral . MyString . StringType) <$> (P.between '"')

-----

varParser :: Parser Expr
varParser = MyVar <$> nameParser

nameParser :: Parser Name
nameParser =
  mkName
    <$> P.predicate
      (P.identifier)
      (\name -> not $ S.member name protectedNames && validName name)

-----

letParser :: Parser Expr
letParser = do
  _ <- P.thenSpace (P.literal "let")
  name <- P.thenSpace nameParser
  _ <- P.thenSpace (P.literal "=")
  expr <- P.thenSpace expressionParser
  _ <- P.thenSpace (P.literal "in")
  inExpr <- expressionParser
  pure (MyLet name expr inExpr)

-----

letPairParser :: Parser Expr
letPairParser = MyLetPair <$> binder1 <*> binder2 <*> equalsParser <*> inParser
  where
    binder1 = do
      _ <- P.thenSpace (P.literal "let")
      _ <- P.literal "("
      _ <- P.space0
      name <- nameParser
      _ <- P.space0
      pure name
    binder2 = do
      _ <- P.literal ","
      _ <- P.space0
      name <- nameParser
      _ <- P.space0
      _ <- P.thenSpace (P.literal ")")
      pure name

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
funcParser = P.thenSpace expressionParser

argParser :: Parser Expr
argParser = literalParser <|> varParser <|> appParser <|> lambdaParser

-----

ifParser :: Parser Expr
ifParser = MyIf <$> predParser <*> thenParser <*> elseParser

predParser :: Parser Expr
predParser = P.right (P.thenSpace (P.literal "if")) expressionParser

thenParser :: Parser Expr
thenParser = P.right (P.thenSpace (P.literal "then")) expressionParser

elseParser :: Parser Expr
elseParser = P.right (P.thenSpace (P.literal "else")) expressionParser

-----

pairParser :: Parser Expr
pairParser = do
  _ <- P.literal "("
  _ <- P.space0
  exprA <- expressionParser
  _ <- P.space0
  _ <- P.literal ","
  _ <- P.space0
  exprB <- expressionParser
  _ <- P.space0
  _ <- P.literal ")"
  _ <- P.space0
  pure $ MyPair exprA exprB

-----

sumParser :: Parser Expr
sumParser = leftParser <|> rightParser
  where
    leftParser = do
      _ <- P.thenSpace (P.literal "Left")
      expr <- expressionParser
      pure (MySum MyLeft expr)
    rightParser = do
      _ <- P.thenSpace (P.literal "Right")
      expr <- expressionParser
      pure (MySum MyRight expr)
