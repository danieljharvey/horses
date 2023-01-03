{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Core.Parser.Types
  ( Parser,
    ParseErrorType,
    ParserExpr,
    protectedNames,
    protectedOperators,
  )
where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Void
import Language.Mimsa.Core.Types.AST
import Language.Mimsa.Core.Types.Identifiers (Name)
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
      "type",
      "match",
      "with",
      "infix",
      "True",
      "False",
      "def",
      "export",
      "import",
      "test"
    ]

protectedOperators :: Set Text
protectedOperators =
  S.fromList
    [ "=",
      "==",
      "+",
      "<>",
      "-",
      "|",
      "++",
      "<=",
      ">=",
      ">",
      "<"
    ]
