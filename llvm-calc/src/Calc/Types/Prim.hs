{-# LANGUAGE DerivingStrategies #-}

module Calc.Types.Prim
  ( Prim (..),
  )
where

newtype Prim
  = PInt Integer
  deriving stock (Eq, Ord, Show)
