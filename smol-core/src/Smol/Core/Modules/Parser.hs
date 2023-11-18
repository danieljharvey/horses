{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Modules.Parser
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
import Smol.Core.Modules.Types.ModuleItem
import Smol.Core.Modules.Types.TestName
import Smol.Core.Parser.DataType (dataTypeParser)
import Smol.Core.Parser.Expr
import Smol.Core.Parser.Identifiers
import Smol.Core.Parser.Primitives (textPrim)
import Smol.Core.Parser.Shared
import Smol.Core.Parser.Type
import Smol.Core.Parser.Typeclass
import Smol.Core.Typecheck.Typeclass.Types
import Smol.Core.Types
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
  try moduleTypeDefinitionParser
    <|> try moduleDefinitionParser
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
-- def oneHundred = 100
-- def id a = a
-- def const a b = a
--
-- top level definition
moduleDefinitionParser :: Parser (ModuleItem Annotation)
moduleDefinitionParser =
  let parser = do
        myString "def"
        ident <- identifierParser
        args <-
          chainl1 ((: []) <$> identifierParser) (pure (<>))
            <|> pure mempty
        myString "="
        (,,) ident args <$> expressionParser
   in withLocation
        ( \ann (ident, args, expr) ->
            ModuleExpression
              ( ModuleExpressionC
                  { meAnn = ann,
                    meIdent = ident,
                    meArgs = args,
                    meExpr = expr
                  }
              )
        )
        parser

-- top level type definition
-- def id : a -> a
-- def compose : (b -> c) -> (a -> b) -> (a -> c)
moduleTypeDefinitionParser :: Parser (ModuleItem Annotation)
moduleTypeDefinitionParser =
  let parser = do
        myString "def"
        ident <- identifierParser
        myString ":"
        constraints <- try typeConstraintParser <|> pure mempty
        (,,) ident constraints <$> typeParser
   in withLocation
        ( \ann (ident, constraints, ty) ->
            ModuleType
              ( ModuleTypeC
                  { mtAnn = ann,
                    mtIdent = ident,
                    mtConstraints = constraints,
                    mtType = ty
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