{-# LANGUAGE OverloadedStrings #-}

module Calc.Parser.Identifier
  ( identifierParser
  )
where

import Control.Monad
import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Calc.Parser.Shared
import Calc.Types
import Text.Megaparsec
import Calc.Parser.Types

protectedNames :: Set Text
protectedNames =
  S.fromList
    [
      "if",
      "then",
      "else",
      "function"
    ]

filterProtectedNames :: Text -> Maybe Text
filterProtectedNames tx =
  if S.member tx protectedNames
    then Nothing
    else Just tx

-- identifier

identifierParser :: Parser Identifier
identifierParser =
  myLexeme identifierParserInternal

-- use this when you are going to wrap myLexeme yourself
identifierParserInternal :: Parser Identifier
identifierParserInternal =
  maybePred
    (takeWhile1P (Just "variable name") Char.isAlphaNum)
    (filterProtectedNames >=> safeMkIdentifier)


