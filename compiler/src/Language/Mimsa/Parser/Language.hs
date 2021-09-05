{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Language
  ( parseExpr,
    parseExprAndFormatError,
    parseAndFormat,
    expressionParser,
    patternMatchParser,
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
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Parser.Literal
import Language.Mimsa.Parser.Pattern
import Language.Mimsa.Parser.RecordAccess
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
        try infixParser
          <|> try literalParser
          <|> try complexParser
          <|> try varParser
          <|> try constructorParser
   in orInBrackets parsers

complexParser :: Parser ParserExpr
complexParser =
  try recordParser
    <|> try arrayParser
    <|> try letParser
    <|> try letPatternParser
    <|> try appParser
    <|> try ifParser
    <|> try pairParser
    <|> try recordAccessParser
    <|> try lambdaParser
    <|> try typeParser
    <|> try patternMatchParser
    <|> try typedHoleParser
    <|> try defineInfixParser

----

letParser :: Parser ParserExpr
letParser = try letInParser <|> letNewlineParser

letNameIn :: Parser Name
letNameIn = do
  _ <- thenSpace (string "let")
  name <- thenSpace nameParser
  _ <- thenSpace (string "=")
  pure name

letInParser :: Parser ParserExpr
letInParser = addLocation $ do
  name <- letNameIn
  boundExpr <- withOptionalSpace expressionParser
  _ <- thenSpace (string "in")
  MyLet mempty name boundExpr <$> expressionParser

letNewlineParser :: Parser ParserExpr
letNewlineParser = addLocation $ do
  name <- letNameIn
  expr <- expressionParser
  _ <- literalWithSpace ";"
  MyLet mempty name expr <$> expressionParser

-----

letPatternParser :: Parser ParserExpr
letPatternParser =
  addLocation $
    do
      _ <- thenSpace (string "let")
      pat <- orInBrackets patternParser
      _ <- thenSpace (string "=")
      expr <- expressionParser
      _ <- withOptionalSpace (string ";" <|> string "in ")
      MyLetPattern mempty pat expr <$> expressionParser

-----

lambdaParser :: Parser ParserExpr
lambdaParser =
  addLocation $
    let slashNameBinder = do
          _ <- string "\\"
          _ <- space
          thenSpace nameParser
        arrowExprBinder = do
          _ <- thenSpace (string "->")
          expressionParser
     in MyLambda mempty <$> slashNameBinder <*> arrowExprBinder

-----

appFunc :: Parser ParserExpr
appFunc =
  try constructorParser
    <|> try recordAccessParser
    <|> try (inBrackets lambdaParser)
    <|> try varParser
    <|> typedHoleParser

expected :: String -> Parser a
expected tx = failure Nothing (S.singleton (Label $ NE.fromList tx))

appParser :: Parser ParserExpr
appParser =
  let parser = do
        cons <- orInBrackets appFunc <|> expected "function"
        exprs <-
          sepBy1
            (withOptionalSpace (orInBrackets consAppArgParser))
            space
            <|> expected "Function argument"
        pure (cons, exprs)
   in withLocation
        ( \loc (cons, exprs) ->
            foldl (MyApp loc) cons exprs
        )
        parser

{-
appParser :: Parser ParserExpr
appParser =
  let parser = do
        func <- appFunc
        _ <- space
        (,) func
          <$> some
            exprInBrackets
   in withLocation
        ( \loc (func, exprs) ->
            foldl (MyApp loc) func exprs
        )
        parser

exprInBrackets :: Parser ParserExpr
exprInBrackets = do
  _ <- string "("
  _ <- space
  expr <- expressionParser
  _ <- space
  _ <- string ")"
  pure expr

-}

-----

recordParser :: Parser ParserExpr
recordParser = withLocation MyRecord $ do
  _ <- string "{"
  _ <- space
  args <- sepBy (withOptionalSpace recordItemParser) (literalWithSpace ",")
  _ <- space
  _ <- string "}"
  pure (M.fromList args)

recordItemParser :: Parser (Name, ParserExpr)
recordItemParser = do
  name <- nameParser
  literalWithSpace ":"
  expr <- withOptionalSpace expressionParser
  pure (name, expr)

-----

ifParser :: Parser ParserExpr
ifParser = addLocation $ do
  _ <- thenSpace (string "if")
  predP <- expressionParser
  _ <- thenSpace (string "then")
  thenP <- expressionParser
  _ <- thenSpace (string "else")
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
  _ <- space
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

-- we don't want to include infix stuff here
consAppArgParser :: Parser ParserExpr
consAppArgParser =
  let parsers =
        try literalParser
          <|> try recordParser
          <|> try arrayParser
          <|> try letParser
          <|> try letPatternParser
          <|> try ifParser
          <|> try pairParser
          <|> try recordAccessParser
          <|> try lambdaParser
          <|> try typeParser
          <|> try typedHoleParser
          <|> try varParser
          <|> constructorParser
   in try (inBrackets infixParser <|> inBrackets appParser)
        <|> orInBrackets parsers

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
  p' <- p
  _ <- space1
  pure p'

infixParser :: Parser ParserExpr
infixParser =
  addLocation
    ( chainl1
        infixExpr
        (MyInfix mempty <$> opParser)
    )

----------

defineInfixParser :: Parser ParserExpr
defineInfixParser = addLocation $ do
  _ <- thenSpace (string "infix")
  infixOp <- thenSpace infixOpParser
  _ <- thenSpace (string "=")
  boundName <- nameParser
  _ <- withOptionalSpace (string ";" <|> string "in ")
  MyDefineInfix mempty infixOp boundName <$> expressionParser

----------

arrayParser :: Parser ParserExpr
arrayParser = withLocation MyArray $ do
  _ <- string "["
  _ <- space
  args <- sepBy (withOptionalSpace expressionParser) (literalWithSpace ",")
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
