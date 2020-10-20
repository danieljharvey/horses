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
  ( Name,
    TyCon,
    TypeName (..),
    safeMkName,
    safeMkTyCon,
  )
import Language.Mimsa.Types.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

type ParseErrorType = ParseErrorBundle Text Void

type ParserExpr = Expr Name Annotation

thenSpace :: Parser a -> Parser a
thenSpace parser = do
  _ <- space
  val <- parser
  _ <- space1
  pure val

-- parse expr, using it all up
parseExpr :: Text -> Either ParseErrorType ParserExpr
parseExpr = parse (expressionParser <* eof) "repl"

parseExprAndFormatError :: Text -> Either Text ParserExpr
parseExprAndFormatError = parseAndFormat (expressionParser <* eof)

parseAndFormat :: Parser a -> Text -> Either Text a
parseAndFormat p = first (T.pack . errorBundlePretty) . parse p "repl"

expressionParser :: Parser ParserExpr
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

literalParser :: Parser ParserExpr
literalParser =
  try boolParser
    <|> try unitParser
    <|> try intParser
    <|> try stringParser

complexParser :: Parser ParserExpr
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

boolParser :: Parser ParserExpr
boolParser = trueParser <|> falseParser

trueParser :: Parser ParserExpr
trueParser = string "True" $> MyLiteral mempty (MyBool True)

falseParser :: Parser ParserExpr
falseParser = string "False" $> MyLiteral mempty (MyBool False)

-----

unitParser :: Parser ParserExpr
unitParser = string "Unit" $> MyLiteral mempty MyUnit

-----

intParser :: Parser ParserExpr
intParser = MyLiteral mempty . MyInt <$> integerParser

-----

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

stringParser :: Parser ParserExpr
stringParser =
  MyLiteral mempty . MyString
    . StringType
    . T.pack
    <$> stringLiteral

-----

varParser :: Parser ParserExpr
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

constructorParser :: Parser ParserExpr
constructorParser = MyConstructor mempty <$> tyConParser

tyConParser :: Parser TyCon
tyConParser =
  maybePred
    identifier
    (inProtected >=> safeMkTyCon)

-----

letParser :: Parser ParserExpr
letParser = try letInParser <|> letNewlineParser

letNameIn :: Parser Name
letNameIn = do
  _ <- thenSpace (string "let")
  name <- thenSpace nameParser
  _ <- thenSpace (string "=")
  pure name

letInParser :: Parser ParserExpr
letInParser = do
  name <- letNameIn
  expr <- withOptionalSpace expressionParser
  _ <- thenSpace (string "in")
  MyLet mempty name expr <$> expressionParser

letNewlineParser :: Parser ParserExpr
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

letPairParser :: Parser ParserExpr
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

equalsParser :: Parser ParserExpr
equalsParser = do
  _ <- thenSpace (string "=")
  thenSpace expressionParser

inParser :: Parser ParserExpr
inParser = do
  _ <- thenSpace (string "in")
  expressionParser

-----

lambdaParser :: Parser ParserExpr
lambdaParser = MyLambda mempty <$> slashNameBinder <*> arrowExprBinder

-- matches \varName
slashNameBinder :: Parser Name
slashNameBinder = do
  _ <- string "\\"
  _ <- space
  thenSpace nameParser

arrowExprBinder :: Parser ParserExpr
arrowExprBinder = do
  _ <- thenSpace (string "->")
  expressionParser

-----

appFunc :: Parser ParserExpr
appFunc =
  try recordAccessParser <|> try (inBrackets lambdaParser)
    <|> varParser

appParser :: Parser ParserExpr
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

exprInBrackets :: Parser ParserExpr
exprInBrackets = do
  literalWithSpace "("
  expr <- expressionParser
  literalWithSpace ")"
  _ <- space
  pure expr

-----

recordParser :: Parser ParserExpr
recordParser = do
  literalWithSpace "{"
  args <- sepBy (withOptionalSpace recordItemParser) (literalWithSpace ",")
  literalWithSpace "}"
  pure (MyRecord mempty (M.fromList args))

recordItemParser :: Parser (Name, ParserExpr)
recordItemParser = do
  name <- nameParser
  literalWithSpace ":"
  expr <- withOptionalSpace expressionParser
  pure (name, expr)

-----

recordAccessParser :: Parser ParserExpr
recordAccessParser =
  try recordAccessParser3
    <|> try recordAccessParser2
    <|> try recordAccessParser1

recordAccessParser1 :: Parser ParserExpr
recordAccessParser1 = do
  expr <- varParser
  name <- dotName
  _ <- space
  pure (MyRecordAccess mempty expr name)

dotName :: Parser Name
dotName = do
  _ <- string "."
  nameParser

recordAccessParser2 :: Parser ParserExpr
recordAccessParser2 = do
  expr <- varParser
  name <- dotName
  name2 <- dotName
  _ <- space
  pure (MyRecordAccess mempty (MyRecordAccess mempty expr name) name2)

recordAccessParser3 :: Parser ParserExpr
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

ifParser :: Parser ParserExpr
ifParser = MyIf mempty <$> predParser <*> thenParser <*> elseParser

predParser :: Parser ParserExpr
predParser = do
  _ <- thenSpace (string "if")
  expressionParser

thenParser :: Parser ParserExpr
thenParser = do
  _ <- thenSpace (string "then")
  expressionParser

elseParser :: Parser ParserExpr
elseParser = do
  _ <- thenSpace (string "else")
  expressionParser

-----

pairParser :: Parser ParserExpr
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

typeParser :: Parser ParserExpr
typeParser =
  MyData mempty
    <$> ( try typeDeclParserWithCons
            <|> try typeDeclParserEmpty
        )
    <*> ( try inExpr
            <|> try inNewLineExpr
        )

inNewLineExpr :: Parser ParserExpr
inNewLineExpr = do
  _ <- literalWithSpace ";"
  expressionParser

inExpr :: Parser ParserExpr
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
constructorAppParser :: Parser ParserExpr
constructorAppParser = do
  cons <- tyConParser
  exprs <-
    sepBy
      (withOptionalSpace (orInBrackets consAppArgParser))
      space
  pure (foldl (MyConsApp mempty) (MyConstructor mempty cons) exprs)

-- we don't want to include infix stuff here
consAppArgParser :: Parser ParserExpr
consAppArgParser =
  let parsers =
        try literalParser
          <|> try complexParser
          <|> try varParser
          <|> constructorParser
   in orInBrackets parsers

----------
caseExprOfParser :: Parser ParserExpr
caseExprOfParser = do
  _ <- thenSpace (string "case")
  sumExpr <- expressionParser
  _ <- thenSpace (string "of")
  pure sumExpr

caseMatchParser :: Parser ParserExpr
caseMatchParser = do
  sumExpr <- caseExprOfParser
  matches <-
    try matchesParser
      <|> pure <$> matchParser
  catchAll <-
    optional (otherwiseParser (not . null $ matches))
  pure $ MyCaseMatch mempty sumExpr matches catchAll

otherwiseParser :: Bool -> Parser ParserExpr
otherwiseParser needsBar = do
  if needsBar
    then () <$ thenSpace (string "|")
    else pure ()
  _ <- thenSpace (string "otherwise")
  expressionParser

matchesParser :: Parser (NonEmpty (TyCon, ParserExpr))
matchesParser =
  NE.fromList
    <$> sepBy
      (withOptionalSpace matchParser)
      (literalWithSpace "|")

matchParser :: Parser (TyCon, ParserExpr)
matchParser = (,) <$> thenSpace tyConParser <*> expressionParser

----------

-- we don't allow super complicate exprs to be used around infix
-- just because it makes awful code and it's slow to parse
infixExpr :: Parser ParserExpr
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

infixParser :: Parser ParserExpr
infixParser = do
  a <- infixExpr
  _ <- space1
  op <- opParser
  _ <- space1
  MyInfix mempty op a <$> infixExpr
