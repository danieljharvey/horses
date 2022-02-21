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
    thenSpace,
  )
where

import Data.Functor (($>))
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifier
import Language.Mimsa.Parser.Identifiers
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
parseExpr = parse (expressionParser <* eof) "repl"

parseExprAndFormatError :: Text -> Either Text ParserExpr
parseExprAndFormatError = parseAndFormat (expressionParser <* eof)

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
  _ <- thenSpace (string "let")
  name <- thenSpace identifierParser
  _ <- thenSpace (string "=")
  pure name

letInParser :: Parser ParserExpr
letInParser = addLocation $ do
  name <- letNameIn
  boundExpr <- optionalSpaceThen expressionParser
  _ <- try (literalWithSpace ";") <|> thenSpace (string "in" $> ())
  MyLet mempty name boundExpr
    <$> optionalSpaceThen expressionParser

letFuncParser :: Parser ParserExpr
letFuncParser = addLocation $ do
  _ <- thenSpace (string "let")
  name <- thenSpace identifierParser
  args <- chainl1 ((: []) <$> thenSpace identifierParser) (pure (<>))
  _ <- thenSpace (string "=")
  expr <- expressionParser
  _ <- try (literalWithSpace ";") <|> thenSpace (string "in" $> ())
  let expr' = foldr (MyLambda mempty) expr args
  MyLet mempty name expr' <$> optionalSpaceThen expressionParser

-----

letPatternParser :: Parser ParserExpr
letPatternParser =
  addLocation $
    do
      _ <- thenSpace (string "let")
      pat <- orInBrackets patternParser
      _ <- thenSpace (string "=")
      expr <- expressionParser
      _ <- optionalSpaceThen (string ";" <|> string "in ")
      MyLetPattern mempty pat expr
        <$> optionalSpaceThen expressionParser

-----

lambdaParser :: Parser ParserExpr
lambdaParser =
  addLocation $
    let slashNameBinder = do
          _ <- string "\\"
          _ <- space
          thenSpace identifierParser
        arrowExprBinder = do
          _ <- thenSpace (string "->")
          expressionParser
     in MyLambda mempty <$> slashNameBinder <*> arrowExprBinder

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
      argParser' = (: []) <$> try (spaceThen argParser)
  args <- chainl1 argParser' (pure (<>))
  pure $ foldl (MyApp mempty) func args

-----

recordParser :: Parser ParserExpr
recordParser = withLocation MyRecord $ do
  let itemParser =
        try recordItemParser
          <|> punnedRecordItemParser
  _ <- string "{"
  _ <- space
  args <- sepBy (optionalSpaceThen itemParser) (string ",")
  _ <- space
  _ <- string "}"
  pure (M.fromList args)

recordItemParser :: Parser (Name, ParserExpr)
recordItemParser = do
  name <- nameParser
  literalWithSpace ":"
  expr <- optionalSpaceThen expressionParser
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
  _ <- string "."
  nameParser

-----

ifParser :: Parser ParserExpr
ifParser = addLocation $ do
  _ <- thenSpace (string "if")
  predP <- expressionParser
  _ <- withSpaces (string "then")
  thenP <- thenSpace expressionParser
  _ <- withSpaces (string "else")
  MyIf mempty predP thenP <$> expressionParser

-----

pairParser :: Parser ParserExpr
pairParser = addLocation $ do
  _ <- string "("
  _ <- space
  exprA <- expressionParser
  _ <- space
  _ <- string ","
  _ <- space
  exprB <- expressionParser
  _ <- space
  _ <- string ")"
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
  _ <- literalWithSpace ";"
  expressionParser

inExpr :: Parser ParserExpr
inExpr = do
  _ <- thenSpace (string "in")
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
    ( inSpaces (string "==")
        $> Equals
    )
    <|> try
      ( inSpaces (string "+")
          $> Add
      )
    <|> try
      ( inSpaces (string "-")
          $> Subtract
      )
    <|> try
      ( inSpaces (string ">")
          $> GreaterThan
      )
    <|> try
      ( inSpaces (string ">=")
          $> GreaterThanOrEqualTo
      )
    <|> try
      ( inSpaces (string "<")
          $> LessThan
      )
    <|> try
      ( inSpaces (string "<=")
          $> LessThanOrEqualTo
      )
    <|> try
      ( inSpaces (string "++")
          $> StringConcat
      )
    <|> try
      ( inSpaces (string "<>")
          $> ArrayConcat
      )
    <|> try
      ( inSpaces
          (Custom <$> infixOpParser)
      )

inSpaces :: Parser a -> Parser a
inSpaces p = do
  _ <- space1
  val <- p
  _ <- space1
  pure val

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
  _ <- thenSpace (string "infix")
  infixOp <- thenSpace infixOpParser
  _ <- thenSpace (string "=")
  boundExpr <- expressionParser
  _ <- optionalSpaceThen (string ";" <|> string "in ")
  MyDefineInfix mempty infixOp boundExpr
    <$> optionalSpaceThen expressionParser

----------

arrayParser :: Parser ParserExpr
arrayParser = withLocation MyArray $ do
  _ <- string "["
  _ <- space
  args <- sepBy (optionalSpaceThen expressionParser) (string ",")
  _ <- space
  _ <- string "]"
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
  _ <- thenSpace (string "match")
  sumExpr <- expressionParser
  _ <- thenSpace (string "with")
  pure sumExpr

patternMatchesParser :: Parser [(ParserPattern, ParserExpr)]
patternMatchesParser =
  sepBy
    patternCaseParser
    (withSpaces "|")

withSpaces :: Parser a -> Parser a
withSpaces p = do
  _ <- space
  p1 <- p
  _ <- space1
  pure p1

patternCaseParser :: Parser (ParserPattern, ParserExpr)
patternCaseParser = do
  pat <- orInBrackets patternParser
  _ <- withSpaces "->"
  patExpr <- expressionParser
  pure (pat, patExpr)

----------
