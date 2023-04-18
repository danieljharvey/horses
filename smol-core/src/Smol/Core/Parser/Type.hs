{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser.Type (parseTypeAndFormatError, parseType, typeParser) where

import Control.Monad ((>=>))
import Control.Monad.Combinators.Expr
  ( Operator (InfixR),
    makeExprParser,
  )
import Data.Bifunctor (first)
import qualified Data.Char as Char
import Data.Foldable (foldl')
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Smol.Core.Parser.Identifiers
  ( identifierParser,
    typeNameParser,
  )
import qualified Smol.Core.Parser.Primitives as Prim
import Smol.Core.Parser.Shared
  ( chainl1,
    commaSep,
    inBrackets,
    maybePred,
    myLexeme,
    myString,
    orInBrackets,
    withLocation,
  )
import Smol.Core.Types.Annotation (Annotation)
import Smol.Core.Types.Identifier
import Smol.Core.Types.ParseDep
import Smol.Core.Types.Type
import Smol.Core.Types.TypeName (TypeName (..))
import Text.Megaparsec
  ( MonadParsec (eof, label, takeWhile1P, try),
    ParseErrorBundle,
    Parsec,
    errorBundlePretty,
    parse,
    sepBy,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void Text

type ParseErrorType = ParseErrorBundle Text Void

parseAndFormat :: Parser a -> Text -> Either Text a
parseAndFormat p = first (T.pack . errorBundlePretty) . parse (p <* eof) "repl"

parseType :: Text -> Either ParseErrorType (ParsedType Annotation)
parseType = parse (space *> typeParser <* eof) "type"

parseTypeAndFormatError :: Text -> Either Text (ParsedType Annotation)
parseTypeAndFormatError = parseAndFormat (space *> typeParser <* eof)

-- | top-level parser for type signatures
typeParser :: Parser (ParsedType Annotation)
typeParser =
  try (orInBrackets tyFunctionParser)
    <|> try tyUnionParser
    <|> simpleTypeParser

-- | all the types except functions
simpleTypeParser :: Parser (ParsedType Annotation)
simpleTypeParser =
  let parsers =
        try tyTupleParser
          <|> typeLiteralParser
          <|> try tyAppParser
          <|> try tVarParser
          <|> try tyPrimitiveParser
          <|> try tyRecordParser
          <|> tyArrayParser
          <|> try adtParser
   in orInBrackets parsers

adtParser :: Parser (ParsedType Annotation)
adtParser =
  try multiDataTypeParser
    <|> monoDataTypeParser

multiDataTypeParser :: Parser (ParsedType Annotation)
multiDataTypeParser = do
  tyName <- typeNameParser
  tyArgs <- some subParser
  pure (dataTypeWithVars mempty tyName tyArgs)

monoDataTypeParser :: Parser (ParsedType Annotation)
monoDataTypeParser = do
  tyName <- typeNameParser
  pure (dataTypeWithVars mempty tyName mempty)

dataTypeWithVars ::
  (Monoid ann) =>
  ann ->
  ParseDep TypeName ->
  [ParsedType ann] ->
  ParsedType ann
dataTypeWithVars ann tyName =
  foldl'
    (TApp mempty)
    (TConstructor ann tyName)

----
typeLiteralParser :: Parser (ParsedType Annotation)
typeLiteralParser =
  label "type literal" $ myLexeme (withLocation TLiteral Prim.typeLiteralParser)

-- | used where a function must be inside brackets for clarity
subParser :: Parser (ParsedType Annotation)
subParser =
  try simpleTypeParser
    <|> try (inBrackets tyFunctionParser)
    <|> try (inBrackets tyUnionParser)

tyPrimitiveParser :: Parser (ParsedType Annotation)
tyPrimitiveParser = TPrim mempty <$> tyPrimParser
  where
    tyPrimParser =
      --      try (myString "String" $> TString)
      try (myString "Bool" $> TPBool)
        <|> try (myString "Int" $> TPInt)
        <|> try (myString "Nat" $> TPNat)

tyAppParser :: Parser (ParsedType Annotation)
tyAppParser = label "type app" $ do
  func <- orInBrackets (TVar mempty . emptyParseDep <$> tyVarParser)
  let argParser' :: Parser [ParsedType Annotation]
      argParser' = (: []) <$> subParser
  args <- chainl1 argParser' (pure (<>))
  pure $ foldl (TApp mempty) func args

tyFunctionParser :: Parser (ParsedType Annotation)
tyFunctionParser = do
  let arrParse :: Operator Parser (ParsedType Annotation)
      arrParse = InfixR $ do
        myString "->"
        pure (TFunc mempty mempty)

  val <- makeExprParser subParser [[arrParse]]
  case val of
    TFunc {} -> pure val
    _ -> fail "don't use function for parsing non-function values"

tyTupleParser :: Parser (ParsedType Annotation)
tyTupleParser = do
  myString "("
  neArgs <- commaSep typeParser
  neTail <- case NE.nonEmpty (NE.tail neArgs) of
    Just ne -> pure ne
    _ -> fail "Expected at least two items in a tuple"
  myString ")"
  pure (TTuple mempty (NE.head neArgs) neTail)

tyIdentifier :: Parser Text
tyIdentifier = myLexeme (takeWhile1P (Just "type variable name") Char.isAlphaNum)

tyUnionParser :: Parser (ParsedType Annotation)
tyUnionParser = do
  chainl1
    simpleTypeParser
    (myString "|" >> pure (TUnion mempty))

inProtectedTypes :: Text -> Maybe Text
inProtectedTypes tx =
  if S.member tx protectedTypeNames
    then Nothing
    else Just tx

-- these names cannot be used as type variables
protectedTypeNames :: Set Text
protectedTypeNames =
  S.fromList
    [ "String",
      "Int",
      "Nat",
      "Bool",
      "Unit",
      "True",
      "False",
      "in",
      "def",
      "type",
      "infix",
      "test",
      "import",
      "export"
    ]

tyVarParser :: Parser Identifier
tyVarParser =
  myLexeme $
    maybePred
      tyIdentifier
      (inProtectedTypes >=> safeMkIdentifier)

tVarParser :: Parser (ParsedType Annotation)
tVarParser = do
  TVar mempty . emptyParseDep <$> tyVarParser

tyRecordParser :: Parser (ParsedType Annotation)
tyRecordParser = withLocation TRecord $ do
  args <- tyRecordArgs
  myString "}"
  pure args

tyRecordArgs :: Parser (Map Identifier (ParsedType Annotation))
tyRecordArgs = do
  myString "{"
  args <- sepBy tyRecordItemParser (myString ",")
  pure (M.fromList args)

tyRecordItemParser :: Parser (Identifier, ParsedType Annotation)
tyRecordItemParser = do
  name <- identifierParser
  myString ":"
  expr <- typeParser
  pure (name, expr)

tyArrayParser :: Parser (ParsedType Annotation)
tyArrayParser = withLocation (`TArray` 0) $ do
  myString "["
  arg <- typeParser
  myString "]"
  pure arg

{-
tyRecordRowParser :: Parser (ParsedType Annotation)
tyRecordRowParser =
  withLocation
    (\loc (args, rest) -> TRecordRow loc args rest)
    ( do
        args <- recordArgs
        myString "|"
        rest <- typeParser
        myString "}"
        pure (args, rest)
    )
-}
