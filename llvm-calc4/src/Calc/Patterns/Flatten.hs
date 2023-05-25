{-# LANGUAGE DerivingStrategies #-}
  {-# LANGUAGE ScopedTypeVariables #-}
module Calc.Patterns.Flatten ( generateMissing) where

import Debug.Trace
import Data.Functor (($>))
import qualified Data.List
import Calc.Types
import qualified Data.List.NonEmpty as NE

-- | given our patterns, generate everything we need minus the ones we have
generateMissing :: (Eq ann, Show ann) => NE.NonEmpty (Pattern ann) -> [Pattern ann]
generateMissing nePats
  = let pats = NE.toList nePats
    in filterMissing pats (generatePatterns pats)

-- | given our patterns, generate any others we might need
generatePatterns :: (Show ann) => [Pattern ann] -> [Pattern ann]
generatePatterns = concatMap generatePattern

-- | given a pattern, generate all other patterns we'll need
generatePattern :: forall ann. (Show ann) => Pattern ann -> [Pattern ann]
generatePattern (PWildcard _) = mempty
generatePattern (PLiteral ann (PBool True)) = [PLiteral ann (PBool False)]
generatePattern (PLiteral ann (PBool False)) = [PLiteral ann (PBool True)]
generatePattern (PTuple ann a as) =
  let genOrOriginal :: Pattern ann -> [Pattern ann]
      genOrOriginal pat =
        traceShowId $ case generatePattern (traceShowId pat) of
          [] -> -- here we want to generate "everything" for the type to stop unnecessary wildcards
              [pat]
          pats -> if isTotal pat then pats else  [pat] <> pats 

      genAs :: [[Pattern ann]]
      genAs = fmap genOrOriginal ([a] <> NE.toList as)

      createTuple :: [Pattern ann] -> Pattern ann
      createTuple items =
          let ne = NE.fromList items
           in PTuple ann (NE.head ne) (NE.fromList $ NE.tail ne)

  in fmap createTuple (sequence genAs)
generatePattern _ = mempty

-- | wildcards are total, vars are total, products are total 
isTotal :: Pattern ann -> Bool
isTotal (PWildcard _) = True
isTotal (PVar _ _) = True
isTotal (PTuple {}) = True
isTotal _ = False

-- filter outstanding items
filterMissing ::
  (Eq ann) =>
  [Pattern ann] ->
  [Pattern ann] ->
  [Pattern ann]
filterMissing patterns required =
  Data.List.nub $ foldr annihiliatePattern required patterns
  where
    annihiliatePattern pat =
      filter
        ( not
            . annihilate
              (removeAnn pat)
            . removeAnn
        )

removeAnn :: Pattern ann -> Pattern ()
removeAnn p = p $> ()

  {-
-- does left pattern satisfy right pattern?
annihilateAll ::
  [(Pattern (), Pattern ())] ->
  Bool
annihilateAll =
  foldr
    (\(a, b) keep -> keep && annihilate a b)
    True
-}

-- | if left is on the right, should we get rid?
annihilate :: Pattern () -> Pattern () -> Bool
annihilate a b | a == b = True
annihilate (PWildcard _) _ = True -- wildcard trumps all
annihilate (PVar _ _) _ = True -- as does var
annihilate _ _as = False


