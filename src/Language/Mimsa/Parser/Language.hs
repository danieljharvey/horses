{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Language
  ( parseExpr,
    parseExprAndFormatError,
    parseAndFormat,
    expressionParser,
    varParser,
    nameParser,
    tyConParser,
    typeDeclParser,
    ParseErrorType,
    Parser,
    thenSpace,
  )
where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Char as Char
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Language.Mimsa.Types
  ( DataType (DataType),
    Expr (..),
    Literal (MyBool, MyInt, MyString, MyUnit),
    Name,
    Operator (..),
    StringType (StringType),
    TyCon,
    TypeName (..),
    safeMkName,
    safeMkTyCon,
  )
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

type ParseErrorType = ParseErrorBundle Text Void

type ParserExpr ann = Expr Name ann

thenSpace :: Parser a -> Parser a
thenSpace parser = do
  _ <- space
  val <- parser
  _ <- space1
  pure val

-- parse expr, using it all up
parseExpr :: Monoid ann => Text -> Either ParseErrorType (ParserExpr ann)
parseExpr = parse (expressionParser <* eof) "repl"

parseExprAndFormatError :: Monoid ann => Text -> Either Text (ParserExpr ann)
parseExprAndFormatError = parseAndFormat (expressionParser <* eof)

parseAndFormat :: Parser a -> Text -> Either Text a
parseAndFormat p = first (T.pack . errorBundlePretty) . parse p "repl"

expressionParser :: Monoid ann => Parser (ParserExpr ann)
expressionParser =
  let parsers =
        try infixParser
          <|> try literalParser
          <|> try complexParser
          <|> try varParser
          <|> try constructorParser
   in orInBrackets parsers

between2 :: Char -> Char -> Parser a -> Parser a
between2 a b parser = do
  _ <- char a
  val <- parser
  _ <- char b
  pure val

inBrackets :: Parser a -> Parser a
inBrackets = between2 '(' ')'

orInBrackets :: Parser a -> Parser a
orInBrackets parser = try parser <|> try (inBrackets parser)

literalParser :: Monoid ann => Parser (ParserExpr ann)
literalParser =
  try boolParser
    <|> try unitParser
    <|> try intParser
    <|> try stringParser

complexParser :: Monoid ann => Parser (ParserExpr ann)
complexParser =
  try letPairParser
    <|> try recordParser
    <|> try letParser
    <|> try ifParser
    <|> try appParser
    <|> try pairParser
    <|> try recordAccessParser
    <|> try lambdaParser
    <|> try typeParser
    <|> try constructorAppParser
    <|> try caseMatchParser

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

integerParser :: Parser Int
integerParser = L.signed space L.decimal

---

boolParser :: Monoid ann => Parser (ParserExpr ann)
boolParser = trueParser <|> falseParser

trueParser :: Monoid ann => Parser (ParserExpr ann)
trueParser = string "True" $> MyLiteral mempty (MyBool True)

falseParser :: Monoid ann => Parser (ParserExpr ann)
falseParser = string "False" $> MyLiteral mempty (MyBool False)

-----

unitParser :: Monoid ann => Parser (ParserExpr ann)
unitParser = string "Unit" $> MyLiteral mempty MyUnit

-----

intParser :: Monoid ann => Parser (ParserExpr ann)
intParser = MyLiteral mempty . MyInt <$> integerParser

-----

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

stringParser :: Monoid ann => Parser (ParserExpr ann)
stringParser =
  MyLiteral mempty . MyString
    . StringType
    . T.pack
    <$> stringLiteral

-----

varParser :: Monoid ann => Parser (ParserExpr ann)
varParser = MyVar mempty <$> nameParser

---

identifier :: Parser Text
identifier = takeWhile1P (Just "variable name") Char.isAlphaNum

maybePred :: (Show a) => Parser a -> (a -> Maybe b) -> Parser b
maybePred parser predicate' = try $ do
  a <- parser
  case predicate' a of
    Just b -> pure b
    _ -> fail $ T.unpack $ "Predicate did not hold for " <> T.pack (show a)

nameParser :: Parser Name
nameParser =
  maybePred
    identifier
    (inProtected >=> safeMkName)

inProtected :: Text -> Maybe Text
inProtected tx =
  if S.member tx protectedNames
    then Nothing
    else Just tx

---

constructorParser :: Monoid ann => Parser (ParserExpr ann)
constructorParser = MyConstructor mempty <$> tyConParser

tyConParser :: Parser TyCon
tyConParser =
  maybePred
    identifier
    (inProtected >=> safeMkTyCon)

-----

letParser :: Monoid ann => Parser (ParserExpr ann)
letParser = try letInParser <|> letNewlineParser

letNameIn :: Parser Name
letNameIn = do
  _ <- thenSpace (string "let")
  name <- thenSpace nameParser
  _ <- thenSpace (string "=")
  pure name

letInParser :: Monoid ann => Parser (ParserExpr ann)
letInParser = do
  name <- letNameIn
  expr <- withOptionalSpace expressionParser
  _ <- thenSpace (string "in")
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
  _ <- space
  name <- nameParser
  _ <- space
  pure name

-----

letPairParser :: Monoid ann => Parser (ParserExpr ann)
letPairParser =
  MyLetPair mempty <$> binder1 <*> binder2
    <*> equalsParser
    <*> inParser
  where
    binder1 = do
      _ <- thenSpace (string "let")
      _ <- string "("
      spacedName
    binder2 = do
      _ <- string ","
      name <- spacedName
      _ <- thenSpace (string ")")
      pure name

-----

equalsParser :: Monoid ann => Parser (ParserExpr ann)
equalsParser = do
  _ <- thenSpace (string "=")
  thenSpace expressionParser

inParser :: Monoid ann => Parser (ParserExpr ann)
inParser = do
  _ <- thenSpace (string "in")
  expressionParser

-----

lambdaParser :: Monoid ann => Parser (ParserExpr ann)
lambdaParser = MyLambda mempty <$> slashNameBinder <*> arrowExprBinder

-- matches \varName
slashNameBinder :: Parser Name
slashNameBinder = do
  _ <- string "\\"
  _ <- space
  thenSpace nameParser

arrowExprBinder :: Monoid ann => Parser (ParserExpr ann)
arrowExprBinder = do
  _ <- thenSpace (string "->")
  expressionParser

-----

appFunc :: Monoid ann => Parser (ParserExpr ann)
appFunc =
  try recordAccessParser <|> try (inBrackets lambdaParser)
    <|> varParser

appParser :: Monoid ann => Parser (ParserExpr ann)
appParser = do
  func <- appFunc
  exprs <- some (withOptionalSpace exprInBrackets)
  pure (foldl (MyApp mempty) func exprs)

literalWithSpace :: Text -> Parser ()
literalWithSpace tx = () <$ withOptionalSpace (string tx)

withOptionalSpace :: Parser a -> Parser a
withOptionalSpace p = do
  _ <- space
  a <- p
  _ <- space
  pure a

exprInBrackets :: Monoid ann => Parser (ParserExpr ann)
exprInBrackets = do
  literalWithSpace "("
  expr <- expressionParser
  literalWithSpace ")"
  _ <- space
  pure expr

-----

recordParser :: Monoid ann => Parser (ParserExpr ann)
recordParser = do
  literalWithSpace "{"
  args <- sepBy (withOptionalSpace recordItemParser) (literalWithSpace ",")
  literalWithSpace "}"
  pure (MyRecord mempty (M.fromList args))

recordItemParser :: Monoid ann => Parser (Name, ParserExpr ann)
recordItemParser = do
  name <- nameParser
  literalWithSpace ":"
  expr <- withOptionalSpace expressionParser
  pure (name, expr)

-----

recordAccessParser :: Monoid ann => Parser (ParserExpr ann)
recordAccessParser =
  try recordAccessParser3
    <|> try recordAccessParser2
    <|> try recordAccessParser1

recordAccessParser1 :: Monoid ann => Parser (ParserExpr ann)
recordAccessParser1 = do
  expr <- varParser
  name <- dotName
  _ <- space
  pure (MyRecordAccess mempty expr name)

dotName :: Parser Name
dotName = do
  _ <- string "."
  nameParser

recordAccessParser2 :: Monoid ann => Parser (ParserExpr ann)
recordAccessParser2 = do
  expr <- varParser
  name <- dotName
  name2 <- dotName
  _ <- space
  pure (MyRecordAccess mempty (MyRecordAccess mempty expr name) name2)

recordAccessParser3 :: Monoid ann => Parser (ParserExpr ann)
recordAccessParser3 = do
  expr <- varParser
  _ <- string "."
  name <- nameParser
  _ <- string "."
  name2 <- nameParser
  _ <- string "."
  name3 <- nameParser
  _ <- space
  pure (MyRecordAccess mempty (MyRecordAccess mempty (MyRecordAccess mempty expr name) name2) name3)

-----

ifParser :: Monoid ann => Parser (ParserExpr ann)
ifParser = MyIf mempty <$> predParser <*> thenParser <*> elseParser

predParser :: Monoid ann => Parser (ParserExpr ann)
predParser = do
  _ <- thenSpace (string "if")
  expressionParser

thenParser :: Monoid ann => Parser (ParserExpr ann)
thenParser = do
  _ <- thenSpace (string "then")
  expressionParser

elseParser :: Monoid ann => Parser (ParserExpr ann)
elseParser = do
  _ <- thenSpace (string "else")
  expressionParser

-----

pairParser :: Monoid ann => Parser (ParserExpr ann)
pairParser = do
  _ <- string "("
  _ <- space
  exprA <- expressionParser
  _ <- space
  _ <- string ","
  _ <- space
  exprB <- expressionParser
  _ <- space
  _ <- string ")"
  _ <- space
  pure $ MyPair mempty exprA exprB

-----

typeParser :: Monoid ann => Parser (ParserExpr ann)
typeParser =
  MyData mempty
    <$> ( try typeDeclParserWithCons
            <|> try typeDeclParserEmpty
        )
    <*> ( try inExpr
            <|> try inNewLineExpr
        )

inNewLineExpr :: Monoid ann => Parser (ParserExpr ann)
inNewLineExpr = do
  _ <- literalWithSpace ";"
  expressionParser

inExpr :: Monoid ann => Parser (ParserExpr ann)
inExpr = do
  _ <- space
  _ <- thenSpace (string "in")
  expressionParser

-----

typeDeclParser :: Parser DataType
typeDeclParser =
  try typeDeclParserWithCons
    <|> try typeDeclParserEmpty

-- it's your "type Void in ..."
typeDeclParserEmpty :: Parser DataType
typeDeclParserEmpty = do
  _ <- thenSpace (string "type")
  tyName <- tyConParser
  pure (DataType tyName mempty mempty)

-- it's your more complex cases
typeDeclParserWithCons :: Parser DataType
typeDeclParserWithCons = do
  _ <- thenSpace (string "type")
  tyName <- thenSpace tyConParser
  tyArgs <- try $ many (thenSpace nameParser)
  _ <- thenSpace (string "=")
  constructors <-
    try manyTypeConstructors
      <|> try oneTypeConstructor
  pure $ DataType tyName tyArgs constructors

--------

----

manyTypeConstructors :: Parser (Map TyCon [TypeName])
manyTypeConstructors = do
  tyCons <-
    sepBy
      (withOptionalSpace oneTypeConstructor)
      (literalWithSpace "|")
  pure (mconcat tyCons)

oneTypeConstructor :: Parser (Map TyCon [TypeName])
oneTypeConstructor = do
  name <- tyConParser
  args <-
    try
      ( do
          _ <- space1
          sepBy (withOptionalSpace typeNameParser) space
      )
      <|> pure mempty
  pure (M.singleton name args)

-----

typeNameParser :: Parser TypeName
typeNameParser =
  try emptyConsParser
    <|> try varNameParser
    <|> try (inBrackets parameterisedConsParser)

-- Simple type like String
emptyConsParser :: Parser TypeName
emptyConsParser = ConsName <$> tyConParser <*> pure mempty

--
parameterisedConsParser :: Parser TypeName
parameterisedConsParser = do
  c <- thenSpace tyConParser
  params <- try (sepBy (withOptionalSpace typeNameParser) space1) <|> pure mempty
  pure $ ConsName c params

varNameParser :: Parser TypeName
varNameParser = VarName <$> nameParser

---
--
constructorAppParser :: Monoid ann => Parser (ParserExpr ann)
constructorAppParser = do
  cons <- tyConParser
  exprs <-
    sepBy
      (withOptionalSpace (orInBrackets consAppArgParser))
      space
  pure (foldl (MyConsApp mempty) (MyConstructor mempty cons) exprs)

-- we don't want to include infix stuff here
consAppArgParser :: Monoid ann => Parser (ParserExpr ann)
consAppArgParser =
  let parsers =
        try literalParser
          <|> try complexParser
          <|> try varParser
          <|> constructorParser
   in orInBrackets parsers

----------
caseExprOfParser :: Monoid ann => Parser (ParserExpr ann)
caseExprOfParser = do
  _ <- thenSpace (string "case")
  sumExpr <- expressionParser
  _ <- thenSpace (string "of")
  pure sumExpr

caseMatchParser :: Monoid ann => Parser (ParserExpr ann)
caseMatchParser = do
  sumExpr <- caseExprOfParser
  matches <-
    try matchesParser
      <|> pure <$> matchParser
  catchAll <-
    optional (otherwiseParser (not . null $ matches))
  pure $ MyCaseMatch mempty sumExpr matches catchAll

otherwiseParser :: Monoid ann => Bool -> Parser (ParserExpr ann)
otherwiseParser needsBar = do
  if needsBar
    then () <$ thenSpace (string "|")
    else pure ()
  _ <- thenSpace (string "otherwise")
  expressionParser

matchesParser :: Monoid ann => Parser (NonEmpty (TyCon, ParserExpr ann))
matchesParser =
  NE.fromList
    <$> sepBy
      (withOptionalSpace matchParser)
      (literalWithSpace "|")

matchParser :: Monoid ann => Parser (TyCon, ParserExpr ann)
matchParser = (,) <$> thenSpace tyConParser <*> expressionParser

----------

-- we don't allow super complicate exprs to be used around infix
-- just because it makes awful code and it's slow to parse
infixExpr :: (Monoid ann) => Parser (ParserExpr ann)
infixExpr =
  let parsers =
        try literalParser
          <|> try recordParser
          <|> try appParser
          <|> try pairParser
          <|> try recordAccessParser
          <|> try constructorAppParser
          <|> try varParser
          <|> constructorParser
   in orInBrackets parsers

opParser :: Parser Operator
opParser = string "==" $> Equals

infixParser :: Monoid ann => Parser (ParserExpr ann)
infixParser = do
  a <- infixExpr
  _ <- space1
  op <- opParser
  _ <- space1
  MyInfix mempty op a <$> infixExpr
