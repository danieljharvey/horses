{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Core.Parser.Language
  ( parseExpr,
    parseMonoType,
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
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)
import GHC.Natural
import Language.Mimsa.Core.Parser.Helpers (addLocation, chainl1, commaSep, inBrackets, orInBrackets, parseAndFormat, withLocation)
import Language.Mimsa.Core.Parser.Identifier
import Language.Mimsa.Core.Parser.Identifiers
import Language.Mimsa.Core.Parser.Lexeme
import Language.Mimsa.Core.Parser.Literal
import Language.Mimsa.Core.Parser.MonoType
import Language.Mimsa.Core.Parser.Pattern
import Language.Mimsa.Core.Parser.TypeDecl
import Language.Mimsa.Core.Parser.Types
import Language.Mimsa.Core.Types.AST
import Language.Mimsa.Core.Types.Identifiers (Name)
import Language.Mimsa.Core.Types.Type
import Text.Megaparsec
import Text.Megaparsec.Char

-- parse expr, using it all up
parseExpr :: Text -> Either ParseErrorType ParserExpr
parseExpr = parse (space *> expressionParser <* eof) "repl"

-- parse monotype
parseMonoType :: Text -> Either ParseErrorType MonoType
parseMonoType = parse (space *> monoTypeParser <* eof) "type"

parseExprAndFormatError :: Text -> Either Text ParserExpr
parseExprAndFormatError = parseAndFormat (space *> expressionParser <* eof)

expressionParser :: Parser ParserExpr
expressionParser =
  let parsers =
        infixParser
          <|> literalParser
          <|> annotationParser
          <|> complexParser
          <|> try globalParser
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
    <|> try tupleAccessParser
    <|> tupleParser
    <|> try recordAccessParser
    <|> recordParser
    <|> lambdaParser
    <|> patternMatchParser
    <|> typedHoleParser

----

letParser :: Parser ParserExpr
letParser = try letInParser <|> letFuncParser

letInParser :: Parser ParserExpr
letInParser = addLocation $ do
  _ <- myString "let"
  (name, anno) <- identifierOrAnnotatedParser
  _ <- myString "="
  boundExpr <- expressionParser
  _ <- try (myString ";") <|> myString "in"
  case anno of
    Just mt ->
      MyLet
        mempty
        name
        (MyAnnotation mempty mt boundExpr)
        <$> expressionParser
    Nothing ->
      MyLet mempty name boundExpr
        <$> expressionParser

letFuncParser :: Parser ParserExpr
letFuncParser = addLocation $ do
  myString "let"
  (name, anno) <- identifierOrAnnotatedParser
  args <- chainl1 ((: []) <$> identifierParser) (pure (<>))
  myString "="
  expr <- expressionParser
  _ <- try (myString ";") <|> myString "in"
  let expr' = foldr (MyLambda mempty) expr args
  case anno of
    Just mt ->
      MyLet mempty name (MyAnnotation mempty mt expr')
        <$> expressionParser
    Nothing ->
      MyLet mempty name expr' <$> expressionParser

identifierOrAnnotatedParser ::
  Parser
    ( Identifier Name Annotation,
      Maybe MonoType
    )
identifierOrAnnotatedParser =
  let regularParser = (,Nothing) <$> identifierParser
      spicyParser = do
        name <- identifierParser
        myString ":"
        mt <- monoTypeParser
        pure (name, Just mt)
   in try (inBrackets spicyParser) <|> regularParser

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
  try recordAccessParser
    <|> try tupleAccessParser
    <|> try globalParser
    <|> try varParser
    <|> try constructorParser
    <|> try annotationParser
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
          <|> try tupleAccessParser
          <|> tupleParser
          <|> try recordAccessParser
          <|> recordParser
          <|> lambdaParser
          <|> typedHoleParser
          <|> try globalParser
          <|> varParser
          <|> constructorParser
   in try (inBrackets infixParser)
        <|> try (inBrackets appParser)
        <|> try annotationParser
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
  pure (name, MyVar mempty Nothing name)

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

tupleAccessParser :: Parser ParserExpr
tupleAccessParser =
  let combine location (tuple, indexes) =
        foldl (MyTupleAccess location) tuple indexes
   in withLocation combine $ do
        tuple <- try varParser <|> try tupleParser
        indexes <- some dotIndex
        pure (tuple, indexes)

dotIndex :: Parser Natural
dotIndex = do
  _ <- myString "."
  natParser

---

ifParser :: Parser ParserExpr
ifParser = addLocation $ do
  _ <- myString "if"
  predP <- expressionParser
  _ <- myString "then"
  thenP <- expressionParser
  _ <- myString "else"
  MyIf mempty predP thenP <$> expressionParser

-----

tupleParser :: Parser ParserExpr
tupleParser = label "tuple" $
  addLocation $ do
    _ <- myString "("
    neArgs <- commaSep expressionParser
    neTail <- case NE.nonEmpty (NE.tail neArgs) of
      Just ne -> pure ne
      _ -> fail "Expected at least two items in a tuple"
    _ <- myString ")"
    pure (MyTuple mempty (NE.head neArgs) neTail)

-----

-- we don't allow super complicate exprs to be used around infix
-- just because it makes awful code and it's slow to parse
infixExpr :: Parser ParserExpr
infixExpr =
  let parsers =
        try literalParser
          <|> try complexParser
          <|> try globalParser
          <|> try varParser
          <|> try annotationParser
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

annotationParser :: Parser ParserExpr
annotationParser =
  let innerParser = do
        expr <- expressionParser
        myString ":"
        MyAnnotation mempty <$> monoTypeParser <*> pure expr
   in addLocation (inBrackets innerParser)
