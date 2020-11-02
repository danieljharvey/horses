{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Parser.Types
  ( Parser,
    ParseErrorType,
    ParserExpr,
    protectedNames,
  )
where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Void
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers (Name)
import Text.Megaparsec

type Parser = Parsec Void Text

type ParseErrorType = ParseErrorBundle Text Void

type ParserExpr = Expr Name Annotation

protectedNames :: Set Text
protectedNames =
  S.fromList
    [ "let",
      "in",
      "if",
      "then",
      "else",
      "case",
      "of",
      "type",
      "otherwise",
      "True",
      "False",
      "Unit"
    ]
