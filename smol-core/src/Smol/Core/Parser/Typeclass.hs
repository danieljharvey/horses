module Smol.Core.Parser.Typeclass
  ( constraintParser,
  )
where

import Data.Text (Text)
import Data.Void
import Smol.Core.Parser.Identifiers
import Smol.Core.Parser.Type
import Smol.Core.Typecheck.Types
import Smol.Core.Types
import Text.Megaparsec hiding (parseTest)

type Parser = Parsec Void Text

constraintParser :: Parser (Constraint ParseDep Annotation)
constraintParser = do
  tcn <- typeclassNameParser
  tys <- many typeParser

  pure $ Constraint tcn tys
