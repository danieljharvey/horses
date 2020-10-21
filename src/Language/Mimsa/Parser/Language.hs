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

import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Parser.Literal
import Language.Mimsa.Parser.RecordAccess
import Language.Mimsa.Parser.TypeDecl
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types (Name, TyCon)
import Language.Mimsa.Types.AST
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

letPairParser :: Parser ParserExpr
letPairParser =
  MyLetPair mempty <$> binder1 <*> binder2
    <*> equalsParser
    <*> inParser
  where
    binder1 = do
      _ <- thenSpace (string "let")
      _ <- string "("
      withOptionalSpace nameParser
    binder2 = do
      _ <- string ","
      name <- withOptionalSpace nameParser
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
    <$> typeDeclParser
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
