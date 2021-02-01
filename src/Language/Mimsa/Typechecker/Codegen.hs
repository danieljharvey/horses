module Language.Mimsa.Typechecker.Codegen (Typeclass (..), typeclassMatches) where

import qualified Data.Map as M
import Data.Monoid
import Language.Mimsa.Types.AST

data Typeclass = Enum
  deriving (Eq, Ord, Show)

-- if we can constructors with no args, it's an enum which we can show
isEnum :: DataType -> Bool
isEnum (DataType _ _ items) = M.size items > 0 && noConsArgs
  where
    noConsArgs :: Bool
    noConsArgs = getAll (foldMap (All . null) (M.elems items))

{-
isNewtype :: DataType -> Bool
isNewtype (DataType _ _ items) = M.size items == 1 &&
-}

typeclassMatches :: DataType -> [Typeclass]
typeclassMatches dt = if isEnum dt then [Enum] else mempty
