{-# LANGUAGE LambdaCase #-}

module Test.Helpers
  ( int,
    bool,
    var,
    tuple,
    patternMatch,
    tyInt,
    tyBool,
    tyTuple,
    patTuple,
    patInt,
    patBool,
  )
where

import Calc
import qualified Data.List.NonEmpty as NE
import Data.String

int :: (Monoid ann) => Integer -> Expr ann
int = EPrim mempty . PInt

bool :: (Monoid ann) => Bool -> Expr ann
bool = EPrim mempty . PBool

var :: (Monoid ann) => String -> Expr ann
var = EVar mempty . Identifier . fromString

tuple :: (Monoid ann) => [Expr ann] -> Expr ann
tuple = \case
  (a : b : rest) -> ETuple mempty a (b NE.:| rest)
  _ -> error "not enough items for tuple"

patternMatch :: (Monoid ann) => Expr ann -> [(Pattern ann, Expr ann)] -> Expr ann
patternMatch matchExpr matches =
  EPatternMatch mempty matchExpr (NE.fromList matches)

tyInt :: (Monoid ann) => Type ann
tyInt = TPrim mempty TInt

tyBool :: (Monoid ann) => Type ann
tyBool = TPrim mempty TBool

tyTuple :: (Monoid ann) => [Type ann] -> Type ann
tyTuple = \case
  (a : b : rest) -> TTuple mempty a (b NE.:| rest)
  _ -> error "not enough items for tyTuple"

patTuple :: (Monoid ann) => [Pattern ann] -> Pattern ann
patTuple = \case
  (a : b : rest) -> PTuple mempty a (b NE.:| rest)
  _ -> error "not enough items for patTuple"

patInt :: (Monoid ann) => Integer -> Pattern ann
patInt = PLiteral mempty . PInt

patBool :: (Monoid ann) => Bool -> Pattern ann
patBool = PLiteral mempty . PBool
