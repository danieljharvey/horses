{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Module
  ( parseModule,
    moduleParser,
    DefPart (..),
  )
where

import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifier
import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Parser.Language
import Language.Mimsa.Parser.Lexeme
import Language.Mimsa.Parser.MonoType
import Language.Mimsa.Types.AST
import Text.Megaparsec
import Text.Megaparsec.Char

parseModule :: Text -> Either ParseErrorType [ModuleItem Annotation]
parseModule = parse (space *> moduleParser <* eof) "repl"

-- currently fails at the first hurdle
-- since we can parse each thing separately, maybe
-- we should be making each throw errors for later, but returning `mempty` so
-- we can collect all of the separate parse errors at once?
-- use `registerParseError` from https://hackage.haskell.org/package/megaparsec-9.2.1/docs/Text-Megaparsec.html
moduleParser :: Parser [ModuleItem Annotation]
moduleParser =
  let bigParsers = parseDef <|> parseType
   in mconcat
        <$> ( chainl1 ((: []) <$> bigParsers) (pure (<>))
                <|> pure mempty
            )

-------

-- type definitions
-- type Maybe a = Just a | Nothing
-- type Tree a = Branch (Tree a) a (Tree a) | Leaf a
parseType :: Parser [ModuleItem Annotation]
parseType = do
  td <- typeDeclParser
  pure [ModuleDataType td]

-------

-- definitions
-- def oneHundred = 100
-- def id a = a
-- def exclaim (str: String) = str ++ "!!!"
-- def exclaim2 (str: String): String = str ++ "!!!"

defPartParser :: Parser (DefPart Annotation)
defPartParser =
  let parseDefArg = DefArg <$> identifierParser
      parseTypeArg =
        inBrackets
          ( do
              name <- identifierParser
              myString ":"
              DefTypedArg name <$> monoTypeParser
          )
      parseDefType = do
        myString ":"
        DefType <$> monoTypeParser
   in parseDefType <|> parseTypeArg <|> parseDefArg

-- top level definition
parseDef :: Parser [ModuleItem Annotation]
parseDef = do
  myString "def"
  name <- nameParser
  parts <-
    chainl1 ((: []) <$> defPartParser) (pure (<>))
      <|> pure mempty
  myString "="
  expr <- expressionParser
  pure [ModuleExpression name parts expr]
