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
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers (Name, TyCon)
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
    <|> try typedHoleParser

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
letInParser =
  let parser = do
        name <- letNameIn
        boundExpr <- withOptionalSpace expressionParser
        _ <- thenSpace (string "in")
        letInExpr <- expressionParser
        pure (name, boundExpr, letInExpr)
   in withLocation
        (\loc (a, b, c) -> MyLet loc a b c)
        parser

letNewlineParser :: Parser ParserExpr
letNewlineParser =
  let parser = do
        name <- letNameIn
        expr <- expressionParser
        _ <- literalWithSpace ";"
        restExpr <- expressionParser
        pure (name, expr, restExpr)
   in withLocation
        (\loc (a, b, c) -> MyLet loc a b c)
        parser

-----

letPairParser :: Parser ParserExpr
letPairParser =
  let binder1 = do
        _ <- thenSpace (string "let")
        _ <- string "("
        withOptionalSpace nameParser
      binder2 = do
        _ <- string ","
        name <- withOptionalSpace nameParser
        _ <- thenSpace (string ")")
        pure name
      parser =
        (,,,) <$> binder1 <*> binder2
          <*> equalsParser
          <*> inParser
   in withLocation
        ( \loc (a, b, c, d) ->
            MyLetPair
              loc
              a
              b
              c
              d
        )
        parser

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
lambdaParser =
  let slashNameBinder = do
        _ <- string "\\"
        _ <- space
        thenSpace nameParser
      arrowExprBinder = do
        _ <- thenSpace (string "->")
        expressionParser
      parser = (,) <$> slashNameBinder <*> arrowExprBinder
   in withLocation
        ( \loc (slash, arrow) ->
            MyLambda loc slash arrow
        )
        parser

-----

appFunc :: Parser ParserExpr
appFunc =
  try recordAccessParser <|> try (inBrackets lambdaParser)
    <|> varParser

appParser :: Parser ParserExpr
appParser =
  let parser = do
        func <- appFunc
        (,) func <$> some (withOptionalSpace exprInBrackets)
   in withLocation
        ( \loc (func, exprs) ->
            foldl (MyApp loc) func exprs
        )
        parser

exprInBrackets :: Parser ParserExpr
exprInBrackets = do
  literalWithSpace "("
  expr <- expressionParser
  literalWithSpace ")"
  _ <- space
  pure expr

-----

recordParser :: Parser ParserExpr
recordParser = withLocation MyRecord $ do
  literalWithSpace "{"
  args <- sepBy (withOptionalSpace recordItemParser) (literalWithSpace ",")
  literalWithSpace "}"
  pure (M.fromList args)

recordItemParser :: Parser (Name, ParserExpr)
recordItemParser = do
  name <- nameParser
  literalWithSpace ":"
  expr <- withOptionalSpace expressionParser
  pure (name, expr)

-----

ifParser :: Parser ParserExpr
ifParser =
  let predParser = do
        _ <- thenSpace (string "if")
        expressionParser
      thenParser = do
        _ <- thenSpace (string "then")
        expressionParser
      elseParser = do
        _ <- thenSpace (string "else")
        expressionParser
      parser =
        (,,) <$> predParser
          <*> thenParser
          <*> elseParser
   in withLocation
        ( \loc (predP, thenP, elseP) ->
            MyIf loc predP thenP elseP
        )
        parser

-----

pairParser :: Parser ParserExpr
pairParser =
  let parser = do
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
        pure (exprA, exprB)
   in withLocation (\loc (a, b) -> MyPair loc a b) parser

-----

typeParser :: Parser ParserExpr
typeParser =
  let parser =
        (,)
          <$> typeDeclParser
          <*> ( try inExpr
                  <|> try inNewLineExpr
              )
   in withLocation
        ( \loc (type', expr') ->
            MyData loc type' expr'
        )
        parser

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
constructorAppParser =
  let parser = do
        cons <- constructorParser
        exprs <-
          sepBy
            (withOptionalSpace (orInBrackets consAppArgParser))
            space
        pure (cons, exprs)
   in withLocation
        ( \loc (cons, exprs) ->
            foldl (MyConsApp loc) cons exprs
        )
        parser

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
caseMatchParser =
  let parser = do
        sumExpr <- caseExprOfParser
        matches <-
          try matchesParser
            <|> pure <$> matchParser
        catchAll <-
          optional (otherwiseParser (not . null $ matches))
        pure (sumExpr, matches, catchAll)
   in withLocation
        ( \loc (sumExpr, matches, catchAll) ->
            MyCaseMatch loc sumExpr matches catchAll
        )
        parser

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
opParser =
  try (string "==" $> Equals)
    <|> try (string "+" $> Add)
    <|> try (string "-" $> Subtract)
    <|> string "<>" $> StringConcat

infixParser :: Parser ParserExpr
infixParser =
  let parser = do
        a <- infixExpr
        _ <- space1
        op <- opParser
        _ <- space1
        b <- infixExpr
        pure (op, a, b)
   in withLocation
        ( \loc (op, a, b) ->
            MyInfix loc op a b
        )
        parser
