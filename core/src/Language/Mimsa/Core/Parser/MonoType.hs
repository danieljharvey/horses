{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Mimsa.Core.Parser.MonoType
  ( monoTypeParser,
    typeDeclParser',
  )
where

import Control.Monad ((>=>))
import Control.Monad.Combinators.Expr
import qualified Data.Char as Char
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Core.Parser.Helpers
import Language.Mimsa.Core.Parser.Identifiers (moduleNameParser, nameParser, typeNameParser)
import Language.Mimsa.Core.Parser.Lexeme
import Language.Mimsa.Core.Parser.Types
import Language.Mimsa.Core.TypeUtils (dataTypeWithVars)
import Language.Mimsa.Core.Types.Identifiers
import Language.Mimsa.Core.Types.Module.ModuleName
import Language.Mimsa.Core.Types.Type
import Text.Megaparsec

-- | top-level parser for type signatures
monoTypeParser :: Parser MonoType
monoTypeParser =
  try (orInBrackets functionParser)
    <|> simpleTypeParser

-- | all the types except functions
simpleTypeParser :: Parser MonoType
simpleTypeParser =
  let parsers =
        try tupleParser
          <|> try varParser
          <|> try primitiveParser
          <|> try recordParser
          <|> try arrayParser
          <|> try dataTypeParser
   in orInBrackets parsers

-- | all the types but prefer no constructor arguments
typeDeclParser' :: Parser MonoType
typeDeclParser' =
  let parsers =
        try tupleParser
          <|> try varParser
          <|> try primitiveParser
          <|> try recordParser
          <|> try arrayParser
          <|> try functionParser
          <|> try monoDataTypeParser
   in try (orInBrackets parsers)
        <|> inBrackets multiDataTypeParser

-- | used where a function must be inside brackets for clarity
subParser :: Parser MonoType
subParser =
  try simpleTypeParser
    <|> try (inBrackets functionParser)

primitiveParser :: Parser MonoType
primitiveParser = MTPrim mempty <$> primParser
  where
    primParser =
      try (myString "String" $> MTString)
        <|> try (myString "Boolean" $> MTBool)
        <|> try (myString "Int" $> MTInt)

arrParse :: Operator Parser MonoType
arrParse = InfixR $ do
  myString "->"
  pure (MTFunction mempty)

functionParser :: Parser MonoType
functionParser = do
  val <- makeExprParser subParser [[arrParse]]
  case val of
    MTFunction {} -> pure val
    _ -> fail "don't use function for parsing non-function values"

tupleParser :: Parser MonoType
tupleParser = do
  myString "("
  neArgs <- commaSep monoTypeParser
  neTail <- case NE.nonEmpty (NE.tail neArgs) of
    Just ne -> pure ne
    _ -> fail "Expected at least two items in a tuple"
  myString ")"
  pure (MTTuple mempty (NE.head neArgs) neTail)

identifier :: Parser Text
identifier = myLexeme (takeWhile1P (Just "type variable name") Char.isAlphaNum)

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
      "Boolean",
      "Unit",
      "in",
      "def",
      "type",
      "infix",
      "test",
      "import",
      "export"
    ]

tyVarParser :: Parser TyVar
tyVarParser =
  myLexeme $
    maybePred
      identifier
      (inProtectedTypes >=> safeMkTyVar)

varParser :: Parser MonoType
varParser = do
  MTVar mempty <$> (TVName <$> tyVarParser)

recordParser :: Parser MonoType
recordParser =
  withLocation
    (\loc (args, rest) -> MTRecord loc args rest)
    ( do
        args <- recordArgs
        rest <- optional $ do
          myString "|"
          monoTypeParser
        myString "}"
        pure (args, rest)
    )

recordArgs :: Parser (Map Name MonoType)
recordArgs = do
  myString "{"
  args <- sepBy recordItemParser (myString ",")
  pure (M.fromList args)

recordItemParser :: Parser (Name, MonoType)
recordItemParser = do
  name <- nameParser
  myString ":"
  expr <- monoTypeParser
  pure (name, expr)

dataTypeParser :: Parser MonoType
dataTypeParser =
  try multiDataTypeParser
    <|> monoDataTypeParser

multiDataTypeParser :: Parser MonoType
multiDataTypeParser = do
  (modName, tyName) <- constructorParser
  tyArgs <- some subParser
  pure (dataTypeWithVars mempty modName tyName tyArgs)

monoDataTypeParser :: Parser MonoType
monoDataTypeParser = do
  (modName, tyName) <- constructorParser
  pure (dataTypeWithVars mempty modName tyName mempty)

----

constructorParser :: Parser (Maybe ModuleName, TypeName)
constructorParser =
  try namespacedConstructorParser
    <|> try plainConstructorParser

plainConstructorParser :: Parser (Maybe ModuleName, TypeName)
plainConstructorParser =
  (Nothing,) <$> typeNameParser

namespacedConstructorParser :: Parser (Maybe ModuleName, TypeName)
namespacedConstructorParser = do
  mName <- moduleNameParser
  myString "."
  (,) (Just mName) <$> typeNameParser

---

arrayParser :: Parser MonoType
arrayParser = do
  myString "["
  arg <- monoTypeParser
  myString "]"
  pure (MTArray mempty arg)
