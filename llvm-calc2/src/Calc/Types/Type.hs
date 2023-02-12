{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
module Calc.Types.Type (Type(..), TypePrim(..)) where

data TypePrim = TBool | TInt
  deriving stock (Eq,Ord,Show)
data Type ann =
  TPrim ann TypePrim
  deriving stock (Eq,Ord,Show,Functor)
