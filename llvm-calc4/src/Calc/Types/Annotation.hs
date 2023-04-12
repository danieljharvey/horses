{-# LANGUAGE DerivingStrategies #-}

module Calc.Types.Annotation
  ( Annotation (..),
  )
where

-- | `Annotation` is used to track source code location
-- it is added to parts of `Expr` during parsing and used to
-- make errors nicer
data Annotation = Location Int Int
  deriving stock (Eq, Ord, Show)

-- | when combining two `Annotation`, take the first one
instance Semigroup Annotation where
  a <> _ = a

-- | Default to an empty `Annotation`
instance Monoid Annotation where
  mempty = Location 0 0
