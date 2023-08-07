{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Parser.Typeclass
  ( constraintParser,
  )
where

import Control.Monad.Identity
import Data.Text (Text)
import Data.Void
import Smol.Core.ExprUtils
import Smol.Core.Parser.Identifiers
import Smol.Core.Parser.Type
import Smol.Core.Typecheck.Types
import Smol.Core.Types
import Text.Megaparsec hiding (parseTest)

type Parser = Parsec Void Text

toIdentity :: Type ParseDep ann -> Type Identity ann
toIdentity = mapTypeDep resolve
  where
    resolve (ParseDep ident _) = Identity ident

constraintParser :: Parser (Constraint Annotation)
constraintParser = do
  tcn <- typeclassNameParser
  tys <- many typeParser

  pure $ Constraint tcn (toIdentity <$> tys)
