{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Language
  ( parseExpr,
    parseExprAndFormatError,
    parseAndFormat,
    expressionParser,
    patternMatchParser,
    recordAccessParser,
    varParser,
    nameParser,
    tyConParser,
    typeDeclParser,
    ParseErrorType,
    Parser,
  )
where

import Data.Functor (($>))
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers (addLocation, chainl1, inBrackets, orInBrackets, parseAndFormat, withLocation)
import Language.Mimsa.Parser.Identifier
import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Parser.Lexeme
import Language.Mimsa.Parser.Literal
import Language.Mimsa.Parser.Pattern
import Language.Mimsa.Parser.TypeDecl
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers (Name)
import Text.Megaparsec
import Text.Megaparsec.Char

-- parse expr, using it all up
parseExpr :: Text -> Either ParseErrorType ParserExpr
parseExpr = parse (space *> expressionParser <* eof) "repl"

parseExprAndFormatError :: Text -> Either Text ParserExpr
parseExprAndFormatError = parseAndFormat (space *> expressionParser <* eof)

expressionParser :: Parser ParserExpr
expressionParser =
  let parsers =
        infixParser
          <|> literalParser
          <|> complexParser
          <|> varParser
          <|> constructorParser
   in orInBrackets parsers

complexParser :: Parser ParserExpr
complexParser =
  arrayParser
    <|> try letParser
    <|> letPatternParser
    <|> try appParser
    <|> ifParser
    <|> pairParser
    <|> try recordAccessParser
    <|> recordParser
    <|> lambdaParser
    <|> typeParser
    <|> patternMatchParser
    <|> typedHoleParser
    <|> defineInfixParser

----

letParser :: Parser ParserExpr
letParser = try letInParser <|> letFuncParser

letNameIn :: Parser (Identifier Name Annotation)
letNameIn = do
  _ <- myString "let"
  name <- identifierParser
  _ <- myString "="
  pure name

letInParser :: Parser ParserExpr
letInParser = addLocation $ do
  name <- letNameIn
  boundExpr <- expressionParser
  _ <- try (myString ";") <|> myString "in"
  MyLet mempty name boundExpr
    <$> expressionParser

letFuncParser :: Parser ParserExpr
letFuncParser = addLocation $ do
  myString "let"
  name <- identifierParser
  args <- chainl1 ((: []) <$> identifierParser) (pure (<>))
  myString "="
  expr <- expressionParser
  _ <- try (myString ";") <|> myString "in"
  let expr' = foldr (MyLambda mempty) expr args
  MyLet mempty name expr' <$> expressionParser

-----

letPatternParser :: Parser ParserExpr
letPatternParser =
  addLocation $
    do
      myString "let"
      pat <- orInBrackets patternParser
      myString "="
      expr <- expressionParser
      myString ";" <|> myString "in"
      MyLetPattern mempty pat expr
        <$> expressionParser

-----

lambdaParser :: Parser ParserExpr
lambdaParser =
  addLocation $ do
    _ <- myString "\\"
    ident <- identifierParser
    _ <- myString "->"
    MyLambda mempty ident <$> expressionParser

-----

appFunc :: Parser ParserExpr
appFunc =
  constructorParser
    <|> try recordAccessParser
    <|> varParser
    <|> try (inBrackets lambdaParser)
    <|> typedHoleParser
    <|> inBrackets appParser

-- we don't want to include infix stuff here
argParser :: Parser ParserExpr
argParser =
  let parsers =
        literalParser
          <|> arrayParser
          <|> letParser
          <|> letPatternParser
          <|> ifParser
          <|> pairParser
          <|> try recordAccessParser
          <|> recordParser
          <|> lambdaParser
          <|> typeParser
          <|> typedHoleParser
          <|> varParser
          <|> constructorParser
   in try (inBrackets infixParser)
        <|> try (inBrackets appParser)
        <|> orInBrackets parsers

appParser :: Parser ParserExpr
appParser = addLocation $ do
  func <- orInBrackets appFunc
  let argParser' :: Parser [ParserExpr]
      argParser' = (: []) <$> argParser
  args <- chainl1 argParser' (pure (<>))
  pure $ foldl (MyApp mempty) func args

-----

recordParser :: Parser ParserExpr
recordParser = withLocation MyRecord $ do
  let itemParser =
        try recordItemParser
          <|> punnedRecordItemParser
  myString "{"
  args <- sepBy itemParser (myString ",")
  myString "}"
  pure (M.fromList args)

recordItemParser :: Parser (Name, ParserExpr)
recordItemParser = do
  name <- nameParser
  myString ":"
  expr <- expressionParser
  pure (name, expr)

punnedRecordItemParser :: Parser (Name, ParserExpr)
punnedRecordItemParser = do
  name <- nameParser
  pure (name, MyVar mempty name)

-----

recordAccessParser :: Parser ParserExpr
recordAccessParser =
  let combine location (record, names) =
        foldl (MyRecordAccess location) record names
   in withLocation combine $ do
        record <- try varParser <|> recordParser
        names <- some dotName
        pure (record, names)

dotName :: Parser Name
dotName = do
  _ <- myString "."
  nameParser

-----

ifParser :: Parser ParserExpr
ifParser = addLocation $ do
  _ <- myString "if"
  predP <- expressionParser
  _ <- myString "then"
  thenP <- expressionParser
  _ <- myString "else"
  MyIf mempty predP thenP <$> expressionParser

-----

pairParser :: Parser ParserExpr
pairParser = addLocation $ do
  _ <- myString "("
  exprA <- expressionParser
  _ <- myString ","
  exprB <- expressionParser
  _ <- myString ")"
  pure (MyPair mempty exprA exprB)

-----

typeParser :: Parser ParserExpr
typeParser =
  addLocation $
    MyData mempty
      <$> typeDeclParser
        <*> (try inExpr <|> try inNewLineExpr)

inNewLineExpr :: Parser ParserExpr
inNewLineExpr = do
  _ <- myString ";"
  expressionParser

inExpr :: Parser ParserExpr
inExpr = do
  _ <- myString "in"
  expressionParser

----------

-- we don't allow super complicate exprs to be used around infix
-- just because it makes awful code and it's slow to parse
infixExpr :: Parser ParserExpr
infixExpr =
  let parsers =
        try literalParser
          <|> try complexParser
          <|> try varParser
          <|> try constructorParser
   in orInBrackets parsers

opParser :: Parser Operator
opParser =
  try
    ( Custom <$> infixOpParser
    )
    <|> try
      ( myString "=="
          $> Equals
      )
    <|> try
      ( myString "-"
          $> Subtract
      )
    <|> try
      ( myString "<>"
          $> ArrayConcat
      )
    <|> try
      ( myString ">="
          $> GreaterThanOrEqualTo
      )
    <|> try
      ( myString "<="
          $> LessThanOrEqualTo
      )
    <|> try
      ( myString ">"
          $> GreaterThan
      )
    <|> try
      ( myString "<"
          $> LessThan
      )
    <|> try
      ( myString "++"
          $> StringConcat
      )
    <|> try
      ( myString "+"
          $> Add
      )

infixParser :: Parser ParserExpr
infixParser =
  addLocation
    ( chainl1
        infixExpr
        ( MyInfix mempty <$> opParser
        )
    )

----------

defineInfixParser :: Parser ParserExpr
defineInfixParser = addLocation $ do
  myString "infix"
  infixOp <- infixOpParser
  myString "="
  boundExpr <- expressionParser
  myString ";" <|> myString "in"
  MyDefineInfix mempty infixOp boundExpr
    <$> expressionParser

----------

arrayParser :: Parser ParserExpr
arrayParser = withLocation MyArray $ do
  myString "["
  args <- sepBy expressionParser (myString ",")
  myString "]"
  pure args

{-

PATTERN MATCHING

pattern matches are of form

match a with
  (Just b) -> b
  _        -> False

-}

patternMatchParser :: Parser ParserExpr
patternMatchParser = addLocation $ do
  matchExpr <- matchExprWithParser
  patterns <-
    try patternMatchesParser
      <|> pure <$> patternCaseParser
  pure $ MyPatternMatch mempty matchExpr patterns

matchExprWithParser :: Parser ParserExpr
matchExprWithParser = do
  myString "match"
  sumExpr <- expressionParser
  myString "with"
  pure sumExpr

patternMatchesParser :: Parser [(ParserPattern, ParserExpr)]
patternMatchesParser =
  sepBy
    patternCaseParser
    (myString "|")

patternCaseParser :: Parser (ParserPattern, ParserExpr)
patternCaseParser = do
  pat <- orInBrackets patternParser
  myString "->"
  patExpr <- expressionParser
  pure (pat, patExpr)

----------
