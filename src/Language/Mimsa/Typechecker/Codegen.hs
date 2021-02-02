module Language.Mimsa.Typechecker.Codegen
  ( Typeclass (..),
    typeclassMatches,
    module Language.Mimsa.Typechecker.Codegen.Newtype,
    module Language.Mimsa.Typechecker.Codegen.Enum,
    module Language.Mimsa.Typechecker.Codegen.Functor,
  )
where

import Data.Either (isRight)
import Language.Mimsa.Typechecker.Codegen.Enum
import Language.Mimsa.Typechecker.Codegen.Functor
import Language.Mimsa.Typechecker.Codegen.Newtype
import Language.Mimsa.Types.AST

data Typeclass
  = Enum
  | Newtype
  | Functor
  deriving (Eq, Ord, Show)

tcPred :: (DataType -> Bool) -> [a] -> DataType -> [a]
tcPred predicate as dt = if predicate dt then as else mempty

typeclassMatches :: DataType -> [Typeclass]
typeclassMatches dt =
  tcPred (isRight . toString) [Enum] dt
    <> tcPred (\a -> isRight (wrap a) && isRight (unwrap a)) [Newtype] dt
    <> tcPred (isRight . functorMap) [Functor] dt
