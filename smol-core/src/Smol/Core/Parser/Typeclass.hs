{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser.Typeclass
  ( constraintParser
  )
where

import Data.Text (Text)
import Data.Void
import Smol.Core.Types
import Text.Megaparsec hiding (parseTest)
import Smol.Core.Typecheck.Types

type Parser = Parsec Void Text

constraintParser :: Parser (Constraint Annotation)
constraintParser =
  fail "sdf"
