{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.LanguageNew
  ( parseExpr,
    expressionParser,
    varParser,
    nameParser,
    tyConParser,
    typeDeclParser,
    ParseErrorType,
  )
where

import Control.Monad ((>=>))
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

sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

thenSpace :: Parser a -> Parser a
thenSpace parser = do
  _ <- space
  val <- parser
  _ <- space1
  pure val

-- parse expr, using it all up
parseExpr :: Monoid ann => Text -> Either ParseErrorType (ParserExpr ann)
parseExpr = parse (expressionParser <* eof) "file.mimsa"

expressionParser :: Monoid ann => Parser (ParserExpr ann)
expressionParser =
  let parsers =
        try literalParser
          <|> try complexParser
          <|> try varParser
          <|> constructorParser
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
orInBrackets parser = try parser <|> inBrackets parser

literalParser :: Monoid ann => Parser (ParserExpr ann)
literalParser =
  try boolParser
    <|> try unitParser
    <|> try intParser
    <|> stringParser

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

integer' :: Parser Int
integer' = lexeme L.decimal

integerParser :: Parser Int
integerParser = L.signed sc integer'

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
letParser = letInParser <|> letNewlineParser

letNameIn :: Parser Name
letNameIn = do
  _ <- thenSpace (string "let")
  name <- thenSpace nameParser
  _ <- thenSpace (string "=")
  pure name

letInParser :: Monoid ann => Parser (ParserExpr ann)
letInParser = do
  name <- letNameIn
  expr <- thenSpace expressionParser
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
recordParser = try fullRecordParser <|> try emptyRecordParser

emptyRecordParser :: Monoid ann => Parser (ParserExpr ann)
emptyRecordParser = do
  literalWithSpace "{"
  literalWithSpace "}"
  pure (MyRecord mempty mempty)

fullRecordParser :: Monoid ann => Parser (ParserExpr ann)
fullRecordParser = do
  literalWithSpace "{"
  args <- some $ do
    item <- recordItemParser
    _ <- string ","
    _ <- space
    pure item
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

typeDeclParser :: Parser DataType
typeDeclParser = typeDeclParserWithCons <|> typeDeclParserEmpty

typeDeclParserEmpty :: Parser DataType
typeDeclParserEmpty = do
  _ <- thenSpace (string "type")
  tyName <- tyConParser
  pure (DataType tyName mempty mempty)

typeDeclParserWithCons :: Parser DataType
typeDeclParserWithCons = do
  _ <- thenSpace (string "type")
  tyName <- thenSpace tyConParser
  tyArgs <- many $ thenSpace nameParser
  _ <- thenSpace (string "=")
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
  _ <- space
  _ <- thenSpace (string "in")
  expressionParser

----

manyTypeConstructors :: Parser (Map TyCon [TypeName])
manyTypeConstructors = do
  cons <- many $ do
    item <- oneTypeConstructor
    _ <- thenSpace (string "|")
    pure item
  lastCons <- oneTypeConstructor
  pure (mconcat cons <> lastCons)

oneTypeConstructor :: Parser (Map TyCon [TypeName])
oneTypeConstructor = do
  name <- tyConParser
  args <- many $ do
    _ <- space1
    typeNameParser
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
  params <- many $ do
    _ <- space1
    typeNameParser
  pure $ ConsName c params

varNameParser :: Parser TypeName
varNameParser = VarName <$> nameParser

---
--
constructorAppParser :: Monoid ann => Parser (ParserExpr ann)
constructorAppParser = do
  cons <- tyConParser
  exprs <- many $ do
    _ <- space1
    orInBrackets expressionParser
  pure (foldl (MyConsApp mempty) (MyConstructor mempty cons) exprs)

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
    matchesParser <|> pure <$> matchParser
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
matchesParser = do
  cons <- many $ do
    val <- matchParser
    _ <- thenSpace (string "|")
    pure val
  lastCons <- matchParser
  pure $ NE.fromList (cons <> [lastCons])

matchParser :: Monoid ann => Parser (TyCon, ParserExpr ann)
matchParser = (,) <$> thenSpace tyConParser <*> expressionParser
