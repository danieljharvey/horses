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

letPairParser :: Parser ParserExpr
letPairParser =
  addLocation $
    let binder1 = do
          _ <- thenSpace (string "let")
          _ <- string "("
          withOptionalSpace nameParser
        binder2 = do
          _ <- string ","
          name <- withOptionalSpace nameParser
          _ <- thenSpace (string ")")
          pure name
     in MyLetPair
          mempty
          <$> binder1
          <*> binder2
          <*> equalsParser
          <*> inParser

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
  try recordAccessParser
    <|> try (inBrackets lambdaParser)
    <|> try varParser
    <|> typedHoleParser

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
   in try (inBrackets infixParser)
        <|> orInBrackets parsers

----------

caseExprOfParser :: Parser ParserExpr
caseExprOfParser = do
  _ <- thenSpace (string "case")
  sumExpr <- expressionParser
  _ <- thenSpace (string "of")
  pure sumExpr

caseMatchParser :: Parser ParserExpr
caseMatchParser = addLocation $ do
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
      ( inSpaces (string "<>")
          $> StringConcat
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
  _ <- withOptionalSpace (string ";")
  MyDefineInfix mempty infixOp boundName <$> expressionParser
