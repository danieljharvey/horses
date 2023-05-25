{-# LANGUAGE DerivingStrategies #-}

module Calc.Patterns.Flatten (SimpleExpr(..),
    SimplePattern(..), flattenPatterns) where

import Data.Bifunctor (first)
import Control.Monad (void)
import Calc.Types
import Data.List.NonEmpty as NE

-- we wish to flatten patterns like this
--
-- case p of (1,2) -> True | (_,_) -> False
--
-- becomes
--
-- case p of
--  [1,b] -> case b of
--                2 -> True
--                _ -> False
--  _ -> False

-- case p of (1,2,3) -> True | (_,_,_) -> False
--
-- becomes
--
-- case p of
--  [1, b, c] -> case b of
--                2 -> case c of
--                       3 -> True
--                       _ -> False
--                _ -> False
--  _ -> False

-- we'll identify missing patterns when we're unable to fill in the fallthrough
-- cases

-- is this it?
data SimplePattern
  = SPTuple Int
  | SPPrim Prim
  | SPWildcard
  deriving stock (Eq,Ord,Show)

data SimpleExpr
  = SEPrim Prim
  | SETupleItem Int SimpleExpr
  | SEPatternMatch SimpleExpr [(SimplePattern, SimpleExpr)]
  deriving stock (Eq,Ord,Show)

-- | this does not bind variables yet
flattenPatterns :: NE.NonEmpty (Pattern ann, Expr ann) -> [(SimplePattern, SimpleExpr )]
flattenPatterns =
  fmap (uncurry flattenPattern . first unrollTuples) . NE.toList

unrollTuples :: Pattern ann -> [Pattern ann]
unrollTuples (PTuple _ a as) = [a] <> NE.toList as
unrollTuples other = [other]

flattenPattern :: [Pattern ann] ->  Expr ann -> (SimplePattern, SimpleExpr)
flattenPattern [PWildcard _] expr = (SPWildcard, simpleExpr expr)
flattenPattern [PLiteral _ prim] expr = (SPPrim prim, simpleExpr expr)
flattenPattern other _ = error $ "flattenPattern " <> show (fmap void other)

simpleExpr :: Expr ann -> SimpleExpr
simpleExpr (EPrim _ prim) = SEPrim prim
simpleExpr other = error $ "simpleExpr " <> show (void other)
