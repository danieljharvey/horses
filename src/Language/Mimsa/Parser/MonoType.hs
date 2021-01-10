{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.MonoType
  ( monoTypeParser,
  )
where

import Control.Monad ((>=>))
import qualified Data.Char as Char
import Data.Functor (($>))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifiers (nameParser)
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Text.Megaparsec
import Text.Megaparsec.Char

-- | top-level parser for type signatures
monoTypeParser :: Parser MonoType
monoTypeParser =
  try (orInBrackets functionParser)
    <|> try simpleTypeParser

-- | all the types except functions
simpleTypeParser :: Parser MonoType
simpleTypeParser =
  let parsers =
        try pairParser
          <|> try varParser
          <|> try primitiveParser
          <|> try dataTypeParser
          <|> try recordParser
   in orInBrackets parsers

-- | used where a function must be inside brackets for clarity
subParser :: Parser MonoType
subParser =
  try (inBrackets functionParser) <|> simpleTypeParser

primitiveParser :: Parser MonoType
primitiveParser = MTPrim mempty <$> primParser
  where
    primParser =
      try (string "String" $> MTString)
        <|> try (string "Boolean" $> MTBool)
        <|> try (string "Int" $> MTInt)
        <|> try (string "Unit" $> MTUnit)

functionParser :: Parser MonoType
functionParser = do
  one <- subParser
  _ <- thenSpace (string "->")
  MTFunction mempty one <$> monoTypeParser

pairParser :: Parser MonoType
pairParser = do
  _ <- string "("
  one <- monoTypeParser
  _ <- literalWithSpace ","
  two <- monoTypeParser
  _ <- string ")"
  pure (MTPair mempty one two)

identifier :: Parser Text
identifier = takeWhile1P (Just "type variable name") Char.isAlphaNum

inProtectedTypes :: Text -> Maybe Text
inProtectedTypes tx =
  if S.member tx protectedTypeNames
    then Nothing
    else Just tx

protectedTypeNames :: Set Text
protectedTypeNames =
  S.fromList
    [ "String",
      "Int",
      "Boolean",
      "Unit"
    ]

tyVarParser :: Parser TyVar
tyVarParser =
  maybePred
    identifier
    (inProtectedTypes >=> safeMkTyVar)

tyConParser :: Parser TyCon
tyConParser =
  maybePred
    identifier
    (inProtectedTypes >=> safeMkTyCon)

varParser :: Parser MonoType
varParser = do
  MTVar mempty <$> (TVName <$> tyVarParser)

recordParser :: Parser MonoType
recordParser = withLocation MTRecord $ do
  literalWithSpace "{"
  args <- sepBy (withOptionalSpace recordItemParser) (literalWithSpace ",")
  literalWithSpace "}"
  pure (M.fromList args)

recordItemParser :: Parser (Name, MonoType)
recordItemParser = do
  name <- nameParser
  literalWithSpace ":"
  expr <- withOptionalSpace monoTypeParser
  pure (name, expr)

dataTypeParser :: Parser MonoType
dataTypeParser =
  try multiDataTypeParser
    <|> monoDataTypeParser

multiDataTypeParser :: Parser MonoType
multiDataTypeParser = do
  tyName <- thenSpace tyConParser
  tyArgs <- try (sepBy1 subParser space1)
  pure (MTData mempty tyName tyArgs)

monoDataTypeParser :: Parser MonoType
monoDataTypeParser = do
  tyName <- tyConParser
  pure (MTData mempty tyName mempty)
