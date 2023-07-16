{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser.Expr (expressionParser) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Void
import Smol.Core.Parser.Identifiers
import Smol.Core.Parser.Op
import Smol.Core.Parser.Pattern
import Smol.Core.Parser.Primitives
import Smol.Core.Parser.Shared
import Smol.Core.Parser.Type
import Smol.Core.Types
import Text.Megaparsec

type Parser = Parsec Void Text

type ParserExpr = Expr ParseDep Annotation

expressionParser :: Parser ParserExpr
expressionParser =
  label "expression" $
    let parsers =
          infixParser
            <|> try literalParser
            <|> try complexParser
            <|> annotationParser
            <|> try varParser
            <|> constructorParser
     in orInBrackets parsers

complexParser :: Parser ParserExpr
complexParser =
  arrayParser
    <|> try letParser
    --    <|> letPatternParser
    <|> try appParser
    <|> ifParser
    <|> try tupleParser
    <|> try recordAccessParser
    <|> recordParser
    <|> lambdaParser
    -- <|> typeParser
    <|> patternMatchParser

-- <|> typedHoleParser
-- <|> defineInfixParser

----

lambdaParser :: Parser ParserExpr
lambdaParser =
  label "lambda" $
    addLocation $ do
      _ <- myString "\\"
      ident <- emptyParseDep <$> identifierParser
      _ <- myString "->"
      ELambda mempty ident <$> expressionParser

-----

appFunc :: Parser ParserExpr
appFunc =
  try recordAccessParser
    <|> try varParser
    <|> constructorParser
    <|> try annotationParser
    <|> try lambdaParser
    -- <|> typedHoleParser
    <|> inBrackets appParser

-- we don't want to include infix stuff here
argParser :: Parser ParserExpr
argParser =
  let parsers =
        literalParser
          --  <|> arrayParser
          <|> letParser
          -- <|> letPatternParser
          <|> ifParser
          <|> tupleParser
          <|> try recordAccessParser
          <|> recordParser
          <|> lambdaParser
          --       <|> typeParser
          --     <|> typedHoleParser
          <|> try varParser
          <|> constructorParser
   in try (inBrackets infixParser)
        <|> try (inBrackets appParser)
        <|> try annotationParser
        <|> orInBrackets parsers

appParser :: Parser ParserExpr
appParser = label "app" $
  addLocation $ do
    func <- orInBrackets appFunc
    let argParser' :: Parser [ParserExpr]
        argParser' = (: []) <$> argParser
    args <- chainl1 argParser' (pure (<>))
    pure $ foldl (EApp mempty) func args

-----

ifParser :: Parser ParserExpr
ifParser = label "if" $
  addLocation $ do
    _ <- myString "if"
    predP <- expressionParser
    _ <- myString "then"
    thenP <- expressionParser
    _ <- myString "else"
    EIf mempty predP thenP <$> expressionParser

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
    pure (ETuple mempty (NE.head neArgs) neTail)

-----

arrayParser :: Parser ParserExpr
arrayParser = withLocation EArray $ do
  myString "["
  args <- Seq.fromList <$> sepBy expressionParser (myString ",")
  myString "]"
  pure args

-----

annotationParser :: Parser ParserExpr
annotationParser =
  let innerParser = do
        expr <- expressionParser
        myString ":"
        EAnn mempty <$> typeParser <*> pure expr
   in label "annotation" $ addLocation (inBrackets innerParser)

literalParser :: Parser ParserExpr
literalParser =
  boolParser
    <|> try natParser
    <|> intParser
    <|> stringParser
    <|> unitExprParser

--    <|> try stringParser

unitExprParser :: Parser ParserExpr
unitExprParser = label "unit" $ myLexeme (withLocation EPrim $ PUnit <$ unitParser)

----

natParser :: Parser ParserExpr
natParser = label "natural" $ myLexeme (withLocation EPrim natPrimParser)

----

intParser :: Parser ParserExpr
intParser = label "integer" $ myLexeme (withLocation EPrim intPrimParser)

---

stringParser :: Parser ParserExpr
stringParser = label "string" $ myLexeme (withLocation EPrim stringPrimParser)

---

boolParser :: Parser ParserExpr
boolParser =
  label "boolean" $
    myLexeme
      ( withLocation
          EPrim
          (truePrimParser <|> falsePrimParser)
      )

-----

letParser :: Parser ParserExpr
letParser = addLocation $ do
  _ <- myString "let"
  ident <- emptyParseDep <$> identifierParser
  _ <- myString "="
  boundExpr <- expressionParser
  _ <- try (myString ";") <|> myString "in"
  ELet mempty ident boundExpr <$> expressionParser

{-
     textPrim :: Parser Text
textPrim = T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))
-}

{-
stringPrim :: Parser Prim
stringPrim =
  MyString . StringType <$> textPrim

stringParser :: Parser ParserExpr
stringParser =
  myLexeme
    ( withLocation
        EPrim
        stringPrim
    )
-}

----

--

recordParser :: Parser ParserExpr
recordParser = withLocation ERecord $ do
  let itemParser =
        try recordItemParser
          <|> punnedRecordItemParser
  myString "{"
  args <- sepBy itemParser (myString ",")
  myString "}"
  pure (M.fromList args)

recordItemParser :: Parser (Identifier, ParserExpr)
recordItemParser = do
  name <- identifierParser
  myString ":"
  expr <- expressionParser
  pure (name, expr)

punnedRecordItemParser :: Parser (Identifier, ParserExpr)
punnedRecordItemParser = do
  name <- identifierParser
  pure (name, EVar mempty (emptyParseDep name))

-----

recordAccessParser :: Parser ParserExpr
recordAccessParser =
  let combine location (record, names) =
        foldl (ERecordAccess location) record names
   in withLocation combine $ do
        record <- try varParser <|> recordParser
        names <- some dotName
        pure (record, names)

dotName :: Parser Identifier
dotName = myString "." >> identifierParser

-- we don't allow super complicate exprs to be used around infix
-- just because it makes awful code and it's slow to parse
infixExpr :: Parser ParserExpr
infixExpr =
  let parsers =
        try literalParser
          <|> try complexParser
          <|> try varParser
          <|> try annotationParser
   in --          <|> try constructorParser
      orInBrackets parsers

infixParser :: Parser ParserExpr
infixParser =
  addLocation
    ( chainl1
        infixExpr
        ( EInfix mempty <$> opParser
        )
    )

{-
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
  case NE.nonEmpty patterns of
    (Just nePatterns) -> pure $ EPatternMatch mempty matchExpr nePatterns
    _ -> error "need at least one pattern"

matchExprWithParser :: Parser ParserExpr
matchExprWithParser = do
  myString "case"
  sumExpr <- expressionParser
  myString "of"
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
