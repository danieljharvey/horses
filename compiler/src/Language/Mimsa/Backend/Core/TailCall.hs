{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Backend.Core.TailCall where

import Language.Mimsa.Types.AST

-- nice

data RecursionType ann
  = NoRecursion
  | TailRecursion
  | NonTailRecursion {rootLetAnn :: ann, nonTailRecurseAnns :: [ann]}
  deriving stock (Eq, Ord, Show)

-- | detect what kind of recursion we are dealing with here
isTailRecursive :: Expr var ann -> RecursionType ann
isTailRecursive _ = NoRecursion
