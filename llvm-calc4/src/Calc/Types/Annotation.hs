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

-- | when combining two `Annotation`, combine to make one big annotation
instance Semigroup Annotation where
  (Location start end) <> (Location start' end') =
      Location (min start start') (max end end')

-- | Default to an empty `Annotation`
instance Monoid Annotation where
  mempty = Location 0 0
