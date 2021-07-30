{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Typechecker.AnnotateExpression (getTypesList, TypedVariable) where

import Data.Functor (($>))
import qualified Data.Map as M
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

-- because of the way our typechecker works, we end up
-- with a single outcome monotype
-- however, perhaps we can use the substitutions generated on the way
-- to get a list of types of other expressions

type TypedVariable = (Name, Type (), Annotation)

getTypesList :: Swaps -> Substitutions -> [TypedVariable]
getTypesList swaps (Substitutions subs) =
  let findType (v, n) = case M.lookup (variableToTypeIdentifier v) subs of
        Just mt -> [(n, mt $> (), getAnnotationForType mt)]
        _ -> []
   in M.toList swaps >>= findType
