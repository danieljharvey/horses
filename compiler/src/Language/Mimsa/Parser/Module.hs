{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Module
  ( parseModule,
    moduleParser,
    exprAndTypeFromParts,
    DefPart (..),
  )
where

import Data.Coerce
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifier
import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Parser.Language
import Language.Mimsa.Parser.Lexeme
import Language.Mimsa.Parser.MonoType
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Identifiers.TypeName
import Language.Mimsa.Types.Typechecker
import Text.Megaparsec
import Text.Megaparsec.Char

parseModule :: Text -> Either ParseErrorType (Module Annotation)
parseModule = parse (space *> moduleParser <* eof) "repl"

-- currently fails at the first hurdle
-- since we can parse each thing separately, maybe
-- we should be making each throw errors for later, but returning `mempty` so
-- we can collect all of the separate parse errors at once?
-- use `registerParseError` from https://hackage.haskell.org/package/megaparsec-9.2.1/docs/Text-Megaparsec.html
moduleParser :: Parser (Module Annotation)
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
parseType :: Parser (Module Annotation)
parseType = do
  td@(DataType tyCon _ _) <- typeDeclParser
  pure (mempty {moDataTypes = M.singleton (coerce tyCon) td})

-------
--
-- definitions
-- def oneHundred = 100
-- def id a = a
-- def exclaim (str: String) = str ++ "!!!"
-- def exclaim2 (str: String): String = str ++ "!!!"

data DefPart ann
  = -- | typeless argument `a`
    DefArg (Identifier Name ann)
  | -- | argument with type `(a: String) ->`
    DefTypedArg (Identifier Name ann) (Type ann)
  | -- | type with no binding `String`
    DefType (Type ann)
  deriving stock (Eq, Ord, Show)

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
parseDef :: Parser (Module Annotation)
parseDef =
  let defP = do
        myString "def"
        name <- nameParser
        parts <-
          chainl1 ((: []) <$> defPartParser) (pure (<>))
            <|> pure mempty
        myString "="
        expr <- expressionParser
        pure (name, parts, expr)
   in defP >>= \(name, parts, expr) -> do
        -- break parts/expr into separate type and expression
        let (defExpr, maybeMt) = exprAndTypeFromParts parts expr
        pure
          ( mempty
              { moExpressions = M.singleton name defExpr,
                moTypeSignatures = maybe mempty (M.singleton name) maybeMt
              }
          )

-- given the bits of things, make a coherent type and expression
-- this feels increasingly like it should belong outside the parser
exprAndTypeFromParts ::
  (Monoid ann) =>
  [DefPart ann] ->
  Expr Name ann ->
  (Expr Name ann, Maybe (Type ann))
exprAndTypeFromParts parts expr =
  let expr' =
        foldr
          ( \part rest -> case part of
              (DefArg ident) -> MyLambda mempty ident rest
              (DefTypedArg ident _) -> MyLambda mempty ident rest
              (DefType _) -> rest
          )
          expr
          parts
      -- if we only have un-typed args, don't bother, we only want them as
      -- placeholders
      filteredParts =
        let includesExplicitTypes =
              any
                ( \case
                    (DefArg _) -> False
                    _ -> True
                )
                parts
            includesReturnType =
              any
                ( \case
                    (DefType _) -> True
                    _ -> False
                )
                parts
         in if includesExplicitTypes
              then
                if includesReturnType
                  then parts
                  else parts <> [DefType (MTVar mempty (TVName "returnType"))]
              else mempty
      mt =
        foldr
          ( \part rest -> case part of
              (DefArg (Identifier _ name)) -> case rest of
                Just rest' ->
                  Just
                    ( MTFunction
                        mempty
                        (MTVar mempty (TVName (coerce name)))
                        rest'
                    )
                Nothing ->
                  Just
                    ( MTVar
                        mempty
                        (TVName (coerce name))
                    )
              (DefTypedArg _ thisMt) -> case rest of
                Just rest' ->
                  Just
                    (MTFunction mempty thisMt rest')
                _ -> Just thisMt
              (DefType thisMt) -> case rest of
                Just rest' ->
                  Just
                    (MTFunction mempty rest' thisMt)
                _ -> Just thisMt
          )
          Nothing
          filteredParts
   in (expr', mt)
