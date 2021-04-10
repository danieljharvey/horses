{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.RecordAccess
  ( recordAccessParser,
  )
where

import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Identifiers
import Language.Mimsa.Parser.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers (Name)
import Text.Megaparsec
import Text.Megaparsec.Char

recordAccessParser :: Parser ParserExpr
recordAccessParser =
  let combine location (record, names) =
        foldl (MyRecordAccess location) record names
   in withLocation combine $ do
        record <- varParser
        names <- some dotName
        pure (record, names)

dotName :: Parser Name
dotName = do
  _ <- string "."
  nameParser
