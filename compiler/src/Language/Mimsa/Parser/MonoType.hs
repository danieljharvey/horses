{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.MonoType
  ( monoTypeParser,
  )
where

import Control.Monad ((>=>))
import Control.Monad.Combinators.Expr
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

-- | used where a function must be inside brackets for clarity
subParser :: Parser MonoType
subParser =
  try simpleTypeParser
    <|> try (inBrackets functionParser)

primitiveParser :: Parser MonoType
primitiveParser = MTPrim mempty <$> primParser
  where
    primParser =
      try (string "String" $> MTString)
        <|> try (string "Boolean" $> MTBool)
        <|> try (string "Int" $> MTInt)
        <|> try (string "Unit" $> MTUnit)

arrParse :: Operator Parser MonoType
arrParse = InfixR $ do
  _ <- space1
  _ <- thenSpace (string "->")
  pure (MTFunction mempty)

functionParser :: Parser MonoType
functionParser = makeExprParser subParser [[arrParse]]

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
  _ <- string "{"
  _ <- space
  args <- recordArgs
  _ <- space
  _ <- string "}"
  pure (M.fromList args)

recordArgs :: Parser [(Name, MonoType)]
recordArgs = sepBy (try $ withOptionalSpace recordItemParser) (literalWithSpace ",")

recordItemParser :: Parser (Name, MonoType)
recordItemParser = do
  name <- nameParser
  literalWithSpace ":"
  expr <- monoTypeParser
  pure (name, expr)

recordRowParser :: Parser MonoType
recordRowParser =
  withLocation
    (\loc (args, rest) -> MTRecordRow loc args rest)
    ( do
        _ <- string "{"
        _ <- space
        args <- recordArgs
        _ <- space
        _ <- string "|"
        _ <- space
        rest <- monoTypeParser
        _ <- space
        _ <- string "}"
        pure (M.fromList args, rest)
    )

dataTypeParser :: Parser MonoType
dataTypeParser =
  try multiDataTypeParser
    <|> try monoDataTypeParser

spaceThen :: Parser a -> Parser a
spaceThen p = do
  _ <- space1
  p

multiDataTypeParser :: Parser MonoType
multiDataTypeParser = do
  tyName <- tyConParser
  tyArgs <- try $ some (spaceThen subParser)
  pure (MTData mempty tyName tyArgs)

monoDataTypeParser :: Parser MonoType
monoDataTypeParser = do
  tyName <- tyConParser
  pure (MTData mempty tyName mempty)

arrayParser :: Parser MonoType
arrayParser = do
  _ <- string "["
  _ <- space
  arg <- monoTypeParser
  _ <- space
  _ <- string "]"
  pure (MTArray mempty arg)
