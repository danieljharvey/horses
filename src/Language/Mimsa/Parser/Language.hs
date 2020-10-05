{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Language
  ( parseExpr,
    parseExpr',
    expressionParser,
    nameParser,
    tyConParser,
    typeDeclParser,
  )
where

import Control.Applicative ((<|>), optional)
import Control.Monad ((>=>))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Parser.Parser (Parser)
import qualified Language.Mimsa.Parser.Parser as P
import Language.Mimsa.Types
  ( DataType (DataType),
    Expr (..),
    Literal (MyBool, MyInt, MyString, MyUnit),
    Name,
    StringType (StringType),
    TyCon,
    TypeName (..),
    safeMkName,
    safeMkTyCon,
  )

type ParserExpr ann = Expr Name ann

-- parse expr, using it all up
parseExpr :: Monoid ann => Text -> Either Text (ParserExpr ann)
parseExpr input = P.runParser (P.thenOptionalSpace expressionParser) input
  >>= \(leftover, a) ->
    if T.length leftover == 0
      then Right a
      else Left ("Leftover input: >>>" <> leftover <> "<<<")

parseExpr' :: Monoid ann => Text -> Either Text (ParserExpr ann)
parseExpr' input = snd <$> P.runParser expressionParser input

failer :: Parser (ParserExpr ann)
failer = P.parseFail (\input -> "Could not parse expression for >>>" <> input <> "<<<")

expressionParser :: Monoid ann => Parser (ParserExpr ann)
expressionParser =
  let parsers =
        literalParser
          <|> complexParser
          <|> varParser
          <|> constructorParser
   in orInBrackets parsers
        <|> failer

inBrackets :: Parser a -> Parser a
inBrackets = P.between2 '(' ')'

orInBrackets :: Parser a -> Parser a
orInBrackets parser = parser <|> inBrackets parser

literalParser :: Monoid ann => Parser (ParserExpr ann)
literalParser =
  boolParser
    <|> unitParser
    <|> intParser
    <|> stringParser

complexParser :: Monoid ann => Parser (ParserExpr ann)
complexParser =
  letPairParser
    <|> recordParser
    <|> letParser
    <|> ifParser
    <|> appParser
    <|> pairParser
    <|> recordAccessParser
    <|> lambdaParser
    <|> typeParser
    <|> constructorAppParser
    <|> caseMatchParser

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
      "type",
      "otherwise",
      "True",
      "False",
      "Unit"
    ]

----

boolParser :: Monoid ann => Parser (ParserExpr ann)
boolParser = trueParser <|> falseParser

trueParser :: Monoid ann => Parser (ParserExpr ann)
trueParser = P.literal "True" $> MyLiteral mempty (MyBool True)

falseParser :: Monoid ann => Parser (ParserExpr ann)
falseParser = P.literal "False" $> MyLiteral mempty (MyBool False)

-----

unitParser :: Monoid ann => Parser (ParserExpr ann)
unitParser = P.literal "Unit" $> MyLiteral mempty MyUnit

-----

intParser :: Monoid ann => Parser (ParserExpr ann)
intParser = MyLiteral mempty . MyInt <$> P.integer

-----

stringParser :: Monoid ann => Parser (ParserExpr ann)
stringParser = MyLiteral mempty . MyString . StringType <$> P.between '"'

-----

varParser :: Monoid ann => Parser (ParserExpr ann)
varParser = MyVar mempty <$> nameParser

---

nameParser :: Parser Name
nameParser =
  P.maybePred
    P.identifier
    (inProtected >=> safeMkName)

inProtected :: Text -> Maybe Text
inProtected tx = if S.member tx protectedNames then Nothing else Just tx

---

constructorParser :: Monoid ann => Parser (ParserExpr ann)
constructorParser = MyConstructor mempty <$> tyConParser

tyConParser :: Parser TyCon
tyConParser =
  P.maybePred
    P.identifier
    (inProtected >=> safeMkTyCon)

-----

letParser :: Monoid ann => Parser (ParserExpr ann)
letParser = letInParser <|> letNewlineParser

letNameIn :: Parser Name
letNameIn = do
  _ <- P.thenSpace (P.literal "let")
  name <- P.thenSpace nameParser
  _ <- P.thenSpace (P.literal "=")
  pure name

letInParser :: Monoid ann => Parser (ParserExpr ann)
letInParser = do
  name <- letNameIn
  expr <- P.thenSpace expressionParser
  _ <- P.thenSpace (P.literal "in")
  MyLet mempty name expr <$> expressionParser

letNewlineParser :: Monoid ann => Parser (ParserExpr ann)
letNewlineParser = do
  name <- letNameIn
  expr <- expressionParser
  _ <- literalWithSpace ";"
  MyLet mempty name expr <$> expressionParser

-----

spacedName :: Parser Name
spacedName = do
  _ <- P.space0
  name <- nameParser
  _ <- P.space0
  pure name

-----

letPairParser :: Monoid ann => Parser (ParserExpr ann)
letPairParser =
  MyLetPair mempty <$> binder1 <*> binder2
    <*> equalsParser
    <*> inParser
  where
    binder1 = do
      _ <- P.thenSpace (P.literal "let")
      _ <- P.literal "("
      spacedName
    binder2 = do
      _ <- P.literal ","
      name <- spacedName
      _ <- P.thenSpace (P.literal ")")
      pure name

-----

equalsParser :: Monoid ann => Parser (ParserExpr ann)
equalsParser =
  P.right (P.thenSpace (P.literal "=")) (P.thenSpace expressionParser)

inParser :: Monoid ann => Parser (ParserExpr ann)
inParser = P.right (P.thenSpace (P.literal "in")) expressionParser

-----

lambdaParser :: Monoid ann => Parser (ParserExpr ann)
lambdaParser = MyLambda mempty <$> slashNameBinder <*> arrowExprBinder

-- matches \varName
slashNameBinder :: Parser Name
slashNameBinder = do
  _ <- P.literal "\\"
  _ <- P.space0
  P.thenSpace nameParser

arrowExprBinder :: Monoid ann => Parser (ParserExpr ann)
arrowExprBinder = P.right (P.thenSpace (P.literal "->")) expressionParser

-----

appFunc :: Monoid ann => Parser (ParserExpr ann)
appFunc = recordAccessParser <|> inBrackets lambdaParser <|> varParser

appParser :: Monoid ann => Parser (ParserExpr ann)
appParser = do
  app <- appFunc
  exprs <- P.oneOrMore (withOptionalSpace exprInBrackets)
  pure (foldl (MyApp mempty) app exprs)

literalWithSpace :: Text -> Parser ()
literalWithSpace tx = () <$ withOptionalSpace (P.literal tx)

withOptionalSpace :: Parser a -> Parser a
withOptionalSpace p = do
  _ <- P.space0
  a <- p
  _ <- P.space0
  pure a

exprInBrackets :: Monoid ann => Parser (ParserExpr ann)
exprInBrackets = do
  literalWithSpace "("
  expr <- expressionParser
  literalWithSpace ")"
  _ <- P.space0
  pure expr

-----

recordParser :: Monoid ann => Parser (ParserExpr ann)
recordParser = fullRecordParser <|> emptyRecordParser

emptyRecordParser :: Monoid ann => Parser (ParserExpr ann)
emptyRecordParser = do
  literalWithSpace "{"
  literalWithSpace "}"
  pure (MyRecord mempty mempty)

fullRecordParser :: Monoid ann => Parser (ParserExpr ann)
fullRecordParser = do
  literalWithSpace "{"
  args <- P.zeroOrMore (P.left recordItemParser (P.left (P.literal ",") P.space0))
  last' <- recordItemParser
  literalWithSpace "}"
  pure (MyRecord mempty (M.fromList $ args <> [last']))

recordItemParser :: Monoid ann => Parser (Name, ParserExpr ann)
recordItemParser = do
  name <- nameParser
  literalWithSpace ":"
  expr <- withOptionalSpace expressionParser
  pure (name, expr)

-----

recordAccessParser :: Monoid ann => Parser (ParserExpr ann)
recordAccessParser =
  recordAccessParser3
    <|> recordAccessParser2
    <|> recordAccessParser1

recordAccessParser1 :: Monoid ann => Parser (ParserExpr ann)
recordAccessParser1 = do
  expr <- varParser
  name <- dotName
  _ <- P.space0
  pure (MyRecordAccess mempty expr name)

dotName :: Parser Name
dotName = do
  _ <- P.literal "."
  nameParser

recordAccessParser2 :: Monoid ann => Parser (ParserExpr ann)
recordAccessParser2 = do
  expr <- varParser
  name <- dotName
  name2 <- dotName
  _ <- P.space0
  pure (MyRecordAccess mempty (MyRecordAccess mempty expr name) name2)

recordAccessParser3 :: Monoid ann => Parser (ParserExpr ann)
recordAccessParser3 = do
  expr <- varParser
  _ <- P.literal "."
  name <- nameParser
  _ <- P.literal "."
  name2 <- nameParser
  _ <- P.literal "."
  name3 <- nameParser
  _ <- P.space0
  pure (MyRecordAccess mempty (MyRecordAccess mempty (MyRecordAccess mempty expr name) name2) name3)

-----

ifParser :: Monoid ann => Parser (ParserExpr ann)
ifParser = MyIf mempty <$> predParser <*> thenParser <*> elseParser

predParser :: Monoid ann => Parser (ParserExpr ann)
predParser = P.right (P.thenSpace (P.literal "if")) expressionParser

thenParser :: Monoid ann => Parser (ParserExpr ann)
thenParser = P.right (P.thenSpace (P.literal "then")) expressionParser

elseParser :: Monoid ann => Parser (ParserExpr ann)
elseParser = P.right (P.thenSpace (P.literal "else")) expressionParser

-----

pairParser :: Monoid ann => Parser (ParserExpr ann)
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
  pure $ MyPair mempty exprA exprB

-----

typeDeclParser :: Parser DataType
typeDeclParser = typeDeclParserWithCons <|> typeDeclParserEmpty

typeDeclParserEmpty :: Parser DataType
typeDeclParserEmpty = do
  _ <- P.thenSpace (P.literal "type")
  tyName <- tyConParser
  pure (DataType tyName mempty mempty)

typeDeclParserWithCons :: Parser DataType
typeDeclParserWithCons = do
  _ <- P.thenSpace (P.literal "type")
  tyName <- P.thenSpace tyConParser
  tyArgs <- P.zeroOrMore (P.left nameParser P.space1)
  _ <- P.thenSpace (P.literal "=")
  constructors <- manyTypeConstructors <|> oneTypeConstructor
  pure $ DataType tyName tyArgs constructors

--------

typeParser :: Monoid ann => Parser (ParserExpr ann)
typeParser =
  MyData mempty <$> (typeDeclParserWithCons <|> typeDeclParserEmpty)
    <*> (inExpr <|> inNewLineExpr)

inNewLineExpr :: Monoid ann => Parser (ParserExpr ann)
inNewLineExpr = do
  _ <- literalWithSpace ";"
  expressionParser

inExpr :: Monoid ann => Parser (ParserExpr ann)
inExpr = do
  _ <- P.space0
  _ <- P.thenSpace (P.literal "in")
  expressionParser

----

manyTypeConstructors :: Parser (Map TyCon [TypeName])
manyTypeConstructors = do
  cons <- NE.toList <$> P.oneOrMore (P.left oneTypeConstructor (P.thenSpace (P.literal "|")))
  lastCons <- oneTypeConstructor
  pure (mconcat cons <> lastCons)

oneTypeConstructor :: Parser (Map TyCon [TypeName])
oneTypeConstructor = do
  name <- tyConParser
  args <- P.zeroOrMore (P.right P.space1 typeNameParser)
  pure (M.singleton name args)

-----

typeNameParser :: Parser TypeName
typeNameParser =
  emptyConsParser <|> varNameParser <|> inBrackets parameterisedConsParser <|> varNameParser

emptyConsParser :: Parser TypeName
emptyConsParser = ConsName <$> tyConParser <*> pure mempty

parameterisedConsParser :: Parser TypeName
parameterisedConsParser = do
  c <- tyConParser
  params <- P.zeroOrMore (P.right P.space1 typeNameParser)
  pure $ ConsName c params

varNameParser :: Parser TypeName
varNameParser = VarName <$> nameParser

---
--
constructorAppParser :: Monoid ann => Parser (ParserExpr ann)
constructorAppParser = do
  cons <- tyConParser
  exprs <- P.oneOrMore (P.right P.space1 (orInBrackets expressionParser))
  pure (foldl (MyConsApp mempty) (MyConstructor mempty cons) exprs)

----------
caseExprOfParser :: Monoid ann => Parser (ParserExpr ann)
caseExprOfParser = do
  _ <- P.thenSpace (P.literal "case")
  sumExpr <- expressionParser
  _ <- P.thenSpace (P.literal "of")
  pure sumExpr

caseMatchParser :: Monoid ann => Parser (ParserExpr ann)
caseMatchParser = do
  sumExpr <- caseExprOfParser
  matches <-
    matchesParser <|> pure <$> matchParser
  catchAll <-
    optional (otherwiseParser (not . null $ matches))
  pure $ MyCaseMatch mempty sumExpr matches catchAll

otherwiseParser :: Monoid ann => Bool -> Parser (ParserExpr ann)
otherwiseParser needsBar = do
  if needsBar
    then () <$ P.thenSpace (P.literal "|")
    else pure ()
  _ <- P.thenSpace (P.literal "otherwise")
  expressionParser

matchesParser :: Monoid ann => Parser (NonEmpty (TyCon, ParserExpr ann))
matchesParser = do
  cons <- P.zeroOrMore (P.left matchParser (P.thenSpace (P.literal "|")))
  lastCons <- matchParser
  pure $ NE.fromList (cons <> [lastCons])

matchParser :: Monoid ann => Parser (TyCon, ParserExpr ann)
matchParser = (,) <$> P.thenSpace tyConParser <*> expressionParser
