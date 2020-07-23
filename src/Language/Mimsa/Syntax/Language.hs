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
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
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

type ParserExpr = Expr Name

-- parse expr, using it all up
parseExpr :: Text -> Either Text ParserExpr
parseExpr input = P.runParser expressionParser input
  >>= \(leftover, a) ->
    if T.length leftover == 0
      then Right a
      else Left ("Leftover input: " <> leftover)

parseExpr' :: Text -> Either Text ParserExpr
parseExpr' input = snd <$> P.runParser expressionParser input

failer :: Parser ParserExpr
failer = P.mkParser (\input -> Left $ "Could not parse expression for >>>" <> input <> "<<<")

expressionParser :: Parser ParserExpr
expressionParser =
  let parsers =
        literalParser
          <|> complexParser
          <|> varParser
   in (P.between2 '(' ')' parsers <|> parsers)
        <|> failer

literalParser :: Parser ParserExpr
literalParser =
  boolParser
    <|> unitParser
    <|> intParser
    <|> stringParser

complexParser :: Parser ParserExpr
complexParser =
  ( letPairParser
      <|> letListParser
      <|> letParser
      <|> ifParser
      <|> lambdaParser
      <|> pairParser
      <|> sumParser
      <|> caseParser
      <|> appParser3
      <|> appParser2
      <|> appParser
      <|> recordAccessParser
      <|> listParser
      <|> recordParser
  )

protectedNames :: Set Text
protectedNames =
  S.fromList
    [ "let",
      "in",
      "if",
      "then",
      "else",
      "case",
      "of",
      "True",
      "False",
      "Unit",
      "Left",
      "Right"
    ]

----

boolParser :: Parser ParserExpr
boolParser = trueParser <|> falseParser

trueParser :: Parser ParserExpr
trueParser = P.literal "True" $> MyLiteral (MyBool True)

falseParser :: Parser ParserExpr
falseParser = P.literal "False" $> MyLiteral (MyBool False)

-----

unitParser :: Parser ParserExpr
unitParser = P.literal "Unit" $> MyLiteral MyUnit

-----

intParser :: Parser ParserExpr
intParser = MyLiteral <$> MyInt <$> P.integer

-----

stringParser :: Parser ParserExpr
stringParser = (MyLiteral . MyString . StringType) <$> (P.between '"')

-----

varParser :: Parser ParserExpr
varParser = MyVar <$> nameParser

nameParser :: Parser Name
nameParser =
  mkName
    <$> P.predicate
      (P.identifier)
      (\name -> not $ S.member name protectedNames && validName name)

-----

letParser :: Parser ParserExpr
letParser = letInParser <|> letNewlineParser

letInParser :: Parser ParserExpr
letInParser = do
  _ <- P.thenSpace (P.literal "let")
  name <- P.thenSpace nameParser
  _ <- P.thenSpace (P.literal "=")
  expr <- P.thenSpace expressionParser
  _ <- P.thenSpace (P.literal "in")
  inExpr <- expressionParser
  pure (MyLet name expr inExpr)

letNewlineParser :: Parser ParserExpr
letNewlineParser = do
  _ <- P.thenSpace (P.literal "let")
  name <- P.thenSpace nameParser
  _ <- P.thenSpace (P.literal "=")
  expr <- expressionParser
  _ <- P.space0
  _ <- P.literal ";"
  _ <- P.space0
  inExpr <- expressionParser
  pure (MyLet name expr inExpr)

-----

letListParser :: Parser ParserExpr
letListParser = MyLetList <$> binder1 <*> binder2 <*> equalsParser <*> inParser
  where
    binder1 = do
      _ <- P.thenSpace (P.literal "let")
      _ <- P.literal "["
      _ <- P.space0
      name <- nameParser
      _ <- P.space0
      pure name
    binder2 = do
      _ <- P.literal ","
      _ <- P.space0
      name <- nameParser
      _ <- P.space0
      _ <- P.thenSpace (P.literal "]")
      pure name

-----

letPairParser :: Parser ParserExpr
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

-----

equalsParser :: Parser ParserExpr
equalsParser =
  P.right (P.thenSpace (P.literal "=")) (P.thenSpace expressionParser)

inParser :: Parser ParserExpr
inParser = P.right (P.thenSpace (P.literal "in")) expressionParser

-----

lambdaParser :: Parser ParserExpr
lambdaParser = MyLambda <$> slashNameBinder <*> arrowExprBinder

-- matches \varName
slashNameBinder :: Parser Name
slashNameBinder = P.right (P.literal "\\") (P.thenSpace nameParser)

arrowExprBinder :: Parser ParserExpr
arrowExprBinder = P.right (P.thenSpace (P.literal "->")) expressionParser

-----

appParser :: Parser ParserExpr
appParser = do
  func <-
    recordAccessParser
      <|> varParser
      <|> lambdaParser
  _ <- P.space0
  _ <- (P.literal "(")
  _ <- P.space0
  arg <- expressionParser
  _ <- P.space0
  _ <- (P.literal ")")
  _ <- P.space0
  pure $ MyApp func arg

appParser2 :: Parser ParserExpr
appParser2 = do
  func <-
    recordAccessParser
      <|> varParser
      <|> lambdaParser
  _ <- P.space0
  _ <- (P.literal "(")
  _ <- P.space0
  arg <- expressionParser
  _ <- P.space0
  _ <- (P.literal ")")
  _ <- P.space0
  _ <- (P.literal "(")
  _ <- P.space0
  arg2 <- expressionParser
  _ <- P.space0
  _ <- (P.literal ")")
  _ <- P.space0
  pure $ MyApp (MyApp func arg) arg2

appParser3 :: Parser ParserExpr
appParser3 = do
  func <- recordAccessParser <|> varParser <|> lambdaParser
  _ <- P.space0
  _ <- (P.literal "(")
  _ <- P.space0
  arg <- expressionParser
  _ <- P.space0
  _ <- (P.literal ")")
  _ <- P.space0
  _ <- (P.literal "(")
  _ <- P.space0
  arg2 <- expressionParser
  _ <- P.space0
  _ <- (P.literal ")")
  _ <- P.space0
  _ <- (P.literal "(")
  _ <- P.space0
  arg3 <- expressionParser
  _ <- P.space0
  _ <- (P.literal ")")
  _ <- P.space0
  pure $ MyApp (MyApp (MyApp func arg) arg2) arg3

-----

listParser :: Parser ParserExpr
listParser = do
  _ <- P.literal "["
  _ <- P.space0
  args <- P.zeroOrMore (P.left expressionParser (P.literal ","))
  last' <- expressionParser
  _ <- P.space0
  _ <- P.literal "]"
  _ <- P.space0
  pure (MyList (NE.fromList (args <> [last'])))

-----

recordParser :: Parser ParserExpr
recordParser = fullRecordParser <|> emptyRecordParser

emptyRecordParser :: Parser ParserExpr
emptyRecordParser = do
  _ <- P.literal "{"
  _ <- P.space0
  _ <- P.literal "}"
  _ <- P.space0
  pure (MyRecord mempty)

fullRecordParser :: Parser ParserExpr
fullRecordParser = do
  _ <- P.literal "{"
  _ <- P.space0
  args <- P.zeroOrMore (P.left recordItemParser (P.left (P.literal ",") P.space0))
  last' <- recordItemParser
  _ <- P.space0
  _ <- P.literal "}"
  _ <- P.space0
  pure (MyRecord (M.fromList $ args <> [last']))

recordItemParser :: Parser (Name, ParserExpr)
recordItemParser = do
  name <- nameParser
  _ <- P.space0
  _ <- P.literal ":"
  _ <- P.space0
  expr <- expressionParser
  _ <- P.space0
  pure (name, expr)

-----

recordAccessParser :: Parser ParserExpr
recordAccessParser =
  recordAccessParser3
    <|> recordAccessParser2
    <|> recordAccessParser1

recordAccessParser1 :: Parser ParserExpr
recordAccessParser1 = do
  expr <- varParser
  _ <- P.literal "."
  name <- nameParser
  _ <- P.space0
  pure (MyRecordAccess expr name)

recordAccessParser2 :: Parser ParserExpr
recordAccessParser2 = do
  expr <- varParser
  _ <- P.literal "."
  name <- nameParser
  _ <- P.literal "."
  name2 <- nameParser
  _ <- P.space0
  pure (MyRecordAccess (MyRecordAccess expr name) name2)

recordAccessParser3 :: Parser ParserExpr
recordAccessParser3 = do
  expr <- varParser
  _ <- P.literal "."
  name <- nameParser
  _ <- P.literal "."
  name2 <- nameParser
  _ <- P.literal "."
  name3 <- nameParser
  _ <- P.space0
  pure (MyRecordAccess (MyRecordAccess (MyRecordAccess expr name) name2) name3)

-----

ifParser :: Parser ParserExpr
ifParser = MyIf <$> predParser <*> thenParser <*> elseParser

predParser :: Parser ParserExpr
predParser = P.right (P.thenSpace (P.literal "if")) expressionParser

thenParser :: Parser ParserExpr
thenParser = P.right (P.thenSpace (P.literal "then")) expressionParser

elseParser :: Parser ParserExpr
elseParser = P.right (P.thenSpace (P.literal "else")) expressionParser

-----

pairParser :: Parser ParserExpr
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

sumParser :: Parser ParserExpr
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

----

caseParser :: Parser ParserExpr
caseParser = do
  _ <- P.thenSpace (P.literal "case")
  sumExpr <- expressionParser
  _ <- P.thenSpace (P.literal "of")
  _ <- P.thenSpace (P.literal "Left")
  leftExpr <- expressionParser
  _ <- P.thenSpace (P.literal "|")
  _ <- P.thenSpace (P.literal "Right")
  rightExpr <- expressionParser
  pure (MyCase sumExpr leftExpr rightExpr)
