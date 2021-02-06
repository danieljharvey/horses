{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Codegen
  ( Typeclass (..),
    typeclassMatches,
    doCodegen,
    module Language.Mimsa.Typechecker.Codegen.Newtype,
    module Language.Mimsa.Typechecker.Codegen.Enum,
    module Language.Mimsa.Typechecker.Codegen.Functor,
  )
where

import Data.Either (isRight)
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Typechecker.Codegen.Enum
import Language.Mimsa.Typechecker.Codegen.Functor
import Language.Mimsa.Typechecker.Codegen.Newtype
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

data Typeclass
  = Enum
  | Newtype
  | Functor
  deriving (Eq, Ord, Show)

tcPred :: (DataType -> Bool) -> [a] -> DataType -> [a]
tcPred predicate as dt =
  if predicate dt
    then as
    else mempty

typeclassMatches :: DataType -> [Typeclass]
typeclassMatches dt =
  tcPred (isRight . toString) [Enum] dt
    <> tcPred
      ( \a ->
          isRight (wrap a)
            && isRight (unwrap a)
      )
      [Newtype]
      dt
    <> tcPred (isRight . functorMap) [Functor] dt

codegenToRow ::
  (DataType -> Either e (Expr Name ())) ->
  Name ->
  DataType ->
  Map Name (Expr Name Annotation)
codegenToRow toDt name dt =
  case toDt dt of
    Right a -> M.singleton name (a $> mempty)
    _ -> mempty

doCodegen :: DataType -> Map Name (Expr Name Annotation)
doCodegen dt =
  codegenToRow toString "toString" dt
    <> codegenToRow wrap "wrap" dt
    <> codegenToRow unwrap "unwrap" dt
    <> codegenToRow functorMap "fmap" dt
