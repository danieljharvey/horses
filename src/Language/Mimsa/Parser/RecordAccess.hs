{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.RecordAccess
  ( recordAccessParser,
  )
where

import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types (Name)
import Language.Mimsa.Types.AST
import Text.Megaparsec
import Text.Megaparsec.Char

recordAccessParser :: Parser ParserExpr
recordAccessParser =
  try recordAccessParser3
    <|> try recordAccessParser2
    <|> try recordAccessParser1

recordAccessParser1 :: Parser ParserExpr
recordAccessParser1 = do
  expr <- varParser
  name <- dotName
  _ <- space
  pure (MyRecordAccess mempty expr name)

dotName :: Parser Name
dotName = do
  _ <- string "."
  nameParser

recordAccessParser2 :: Parser ParserExpr
recordAccessParser2 = do
  expr <- varParser
  name <- dotName
  name2 <- dotName
  _ <- space
  pure (MyRecordAccess mempty (MyRecordAccess mempty expr name) name2)

recordAccessParser3 :: Parser ParserExpr
recordAccessParser3 = do
  expr <- varParser
  _ <- string "."
  name <- nameParser
  _ <- string "."
  name2 <- nameParser
  _ <- string "."
  name3 <- nameParser
  _ <- space
  pure (MyRecordAccess mempty (MyRecordAccess mempty (MyRecordAccess mempty expr name) name2) name3)
