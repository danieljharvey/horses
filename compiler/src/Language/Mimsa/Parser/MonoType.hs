{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.MonoType
  ( monoTypeParser,
    typeDeclParser',
  )
where

import Control.Monad ((>=>))
import Control.Monad.Combinators.Expr
import qualified Data.Char as Char
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifiers (nameParser)
import Language.Mimsa.Parser.Lexeme
import Language.Mimsa.Parser.Types
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
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
        try pairParser
          <|> try varParser
          <|> try primitiveParser
          <|> try recordRowParser
          <|> try recordParser
          <|> try arrayParser
          <|> try dataTypeParser
   in orInBrackets parsers

-- | all the types but prefer no constructor arguments
typeDeclParser' :: Parser MonoType
typeDeclParser' =
  let parsers =
        try pairParser
          <|> try varParser
          <|> try primitiveParser
          <|> try recordRowParser
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

pairParser :: Parser MonoType
pairParser = do
  myString "("
  one <- monoTypeParser
  myString ","
  two <- monoTypeParser
  myString ")"
  pure (MTPair mempty one two)

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
      "in"
    ]

tyVarParser :: Parser TyVar
tyVarParser =
  myLexeme $
    maybePred
      identifier
      (inProtectedTypes >=> safeMkTyVar)

tyConParser :: Parser TyCon
tyConParser =
  myLexeme $
    maybePred
      identifier
      (inProtectedTypes >=> safeMkTyCon)

varParser :: Parser MonoType
varParser = do
  MTVar mempty <$> (TVName Nothing <$> tyVarParser)

recordParser :: Parser MonoType
recordParser = withLocation MTRecord $ do
  args <- recordArgs
  myString "}"
  pure args

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

recordRowParser :: Parser MonoType
recordRowParser =
  withLocation
    (\loc (args, rest) -> MTRecordRow loc args rest)
    ( do
        args <- recordArgs
        myString "|"
        rest <- monoTypeParser
        myString "}"
        pure (args, rest)
    )

dataTypeParser :: Parser MonoType
dataTypeParser =
  try multiDataTypeParser
    <|> monoDataTypeParser

multiDataTypeParser :: Parser MonoType
multiDataTypeParser = do
  tyName <- tyConParser
  tyArgs <- some subParser
  pure (dataTypeWithVars mempty tyName tyArgs)

monoDataTypeParser :: Parser MonoType
monoDataTypeParser = do
  tyName <- tyConParser
  pure (dataTypeWithVars mempty tyName mempty)

arrayParser :: Parser MonoType
arrayParser = do
  myString "["
  arg <- monoTypeParser
  myString "]"
  pure (MTArray mempty arg)
