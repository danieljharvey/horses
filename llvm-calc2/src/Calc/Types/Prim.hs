{-# LANGUAGE DerivingStrategies #-}

module Calc.Types.Prim
  ( Prim (..),
  )
where

data Prim
  = PInt Integer
  | PBool Bool
  deriving stock (Eq, Ord, Show)
