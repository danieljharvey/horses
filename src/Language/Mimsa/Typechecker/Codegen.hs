module Language.Mimsa.Typechecker.Codegen (Typeclass (..), typeclassMatches) where

import Language.Mimsa.Types.AST

data Typeclass = Show
  deriving (Eq, Ord, Show)

typeclassMatches :: DataType -> [Typeclass]
typeclassMatches _ = mempty
