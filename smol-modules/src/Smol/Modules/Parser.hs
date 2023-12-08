{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Modules.Parser
  ( moduleParser,
    parseModule,
    parseModuleAndFormatError,
  )
where

import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Smol.Core.Parser.DataType (dataTypeParser)
import Smol.Core.Parser.Expr
import Smol.Core.Parser.Identifiers
import Smol.Core.Parser.Primitives (textPrim)
import Smol.Core.Parser.Shared
import Smol.Core.Parser.Type
import Smol.Core.Parser.Typeclass
import Smol.Core.Typecheck.Typeclass.Types
import Smol.Core.Types
import Smol.Modules.Types.ModuleItem
import Smol.Modules.Types.TestName
import Text.Megaparsec hiding (parseTest)
import Text.Megaparsec.Char

type Parser = Parsec Void Text

type ParseErrorType = ParseErrorBundle Text Void

parseAndFormat :: Parser a -> Text -> Either Text a
parseAndFormat p = first (T.pack . errorBundlePretty) . parse (p <* eof) "repl"

parseModule :: Text -> Either ParseErrorType [ModuleItem Annotation]
parseModule = parse (space *> moduleParser <* eof) "repl"

parseModuleAndFormatError :: Text -> Either Text [ModuleItem Annotation]
parseModuleAndFormatError = parseAndFormat (space *> moduleParser <* eof)

-------

-- currently fails at the first hurdle
-- since we can parse each thing separately, maybe
-- we should be making each throw errors for later, but returning `mempty` so
-- we can collect all of the separate parse errors at once?
-- use `registerParseError` from https://hackage.haskell.org/package/megaparsec-9.2.1/docs/Text-Megaparsec.html
moduleParser :: Parser [ModuleItem Annotation]
moduleParser =
  mconcat
    <$> ( chainl1 ((\a -> [[a]]) <$> parseModuleItem) (pure (<>))
            <|> pure mempty
        )

-- we've excluded Export here
parseModuleItem :: Parser (ModuleItem Annotation)
parseModuleItem =
    try moduleDefinitionParser
    <|> try moduleTypeDeclarationParser
    <|> parseTest
    <|> parseInstance
    <|> parseClass

-------

-- type definitions
-- type Maybe a = Just a | Nothing
-- type Tree a = Branch (Tree a) a (Tree a) | Leaf a
moduleTypeDeclarationParser :: Parser (ModuleItem Annotation)
moduleTypeDeclarationParser =
  withLocation
    ( \ann dt ->
        ModuleDataType
          ( ModuleDataTypeC
              { mdtAnn = ann,
                mdtDataType = dt
              }
          )
    )
    dataTypeParser

-------

-- definitions
-- def oneHundred : Integer = 100
-- def id (a: a) : a = a
-- def const (a: a) (b: b): a = a
-- def withEq (Eq a) => (one: a) (two: a): Boolean { equals one two }
-- top level definition
moduleDefinitionParser :: Parser (ModuleItem Annotation)
moduleDefinitionParser =
  let argPairParser = do
          myString "("
          ident <- identifierParser
          myString ":"
          ty <- typeParser
          myString ")"
          pure (ident, ty)
      parser = do
        myString "def"
        ident <- identifierParser
        constraints <- try typeConstraintParser <|> pure mempty
        args <-
          chainl1 ((: []) <$> argPairParser) (pure (<>))
            <|> pure mempty
        myString ":"
        retType <- typeParser
        myString "{"
        expr <- expressionParser
        myString "}"
        pure (ident, constraints, args, retType, expr)
   in withLocation
        ( \ann (ident, constraints, args, retType, expr) ->
            ModuleExpression
              ( ModuleExpressionC
                  { meAnn = ann,
                    meIdent = ident,
                    meConstraints = constraints,
                    meArgs = args,
                    meReturnType = retType,
                    meExpr = expr
                  }
              )
        )
        parser

typeConstraintParser :: Parser [Constraint ParseDep Annotation]
typeConstraintParser = do
  myString "("
  constraints <- commaSep constraintParser
  myString ")"
  myString "=>"
  pure (NE.toList constraints)

-- `test "everything is fine" { 1 + 1 == 2 }`
parseTest :: Parser (ModuleItem Annotation)
parseTest = do
  myString "test"
  testName <- testNameParser
  myString "{"
  expr <- expressionParser
  myString "}"
  pure $ ModuleTest testName expr

-- `instance Eq Int { \a -> \b -> a == b }`
parseInstance :: Parser (ModuleItem Annotation)
parseInstance =
  let parser = do
        myString "instance"
        constraints <- try typeConstraintParser <|> pure mempty
        mainConstraint <- constraintParser
        myString "{"
        expr <- expressionParser
        myString "}"
        pure (constraints, mainConstraint, expr)
   in withLocation
        ( \ann (constraints, head', expr) ->
            ModuleInstance
              ( ModuleInstanceC
                  { miAnn = ann,
                    miConstraints = constraints,
                    miHead = head',
                    miExpr = expr
                  }
              )
        )
        parser

parseClass :: Parser (ModuleItem Annotation)
parseClass = do
  myString "class"
  typeclassName <- typeclassNameParser
  parts <-
    chainl1 ((: []) <$> identifierParser) (pure (<>))
      <|> pure mempty
  myString "{"
  fnName <- identifierParser
  myString ":"
  ty <- typeParser
  myString "}"

  pure $
    ModuleClass
      ( Typeclass
          { tcName = typeclassName,
            tcArgs = parts,
            tcFuncName = fnName,
            tcFuncType = ty
          }
      )

testNameParser :: Parser TestName
testNameParser = myLexeme $ TestName <$> textPrim
