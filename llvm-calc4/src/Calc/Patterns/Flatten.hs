{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Calc.Patterns.Flatten (generateMissing) where

import Calc.PatternUtils (getPatternAnnotation)
import Calc.Types
import Data.Functor (($>))
import qualified Data.List
import qualified Data.List.NonEmpty as NE

-- | given our patterns, generate everything we need minus the ones we have
generateMissing :: (Show ann) => NE.NonEmpty (Pattern (Type ann)) -> [Pattern ()]
generateMissing nePats =
  let pats = NE.toList nePats
   in filterMissing (removeAnn <$> pats) (generatePatterns pats)

-- | given our patterns, generate any others we might need
generatePatterns :: (Show ann) => [Pattern (Type ann)] -> [Pattern ()]
generatePatterns = concatMap generatePattern

generateForType :: Type ann -> [Pattern ()]
generateForType (TPrim _ TBool) = [PLiteral () (PBool True), PLiteral () (PBool False)]
generateForType (TPrim _ TInt) = [PWildcard ()] -- too many, just do wildcard
generateForType _ = [PWildcard ()]

typeIsTotal :: Type ann -> Bool
typeIsTotal (TPrim {}) = False
typeIsTotal (TTuple {}) = True
typeIsTotal (TFunction {}) = True

-- | given a pattern, generate all other patterns we'll need
generatePattern :: forall ann. (Show ann) => Pattern (Type ann) -> [Pattern ()]
generatePattern (PWildcard _) = mempty
generatePattern (PLiteral _ (PBool True)) = [PLiteral () (PBool False)]
generatePattern (PLiteral _ (PBool False)) = [PLiteral () (PBool True)]
generatePattern (PTuple _ a as) =
  let genOrOriginal :: Pattern (Type ann) -> [Pattern ()]
      genOrOriginal pat =
        case generatePattern pat of
          [] ->
            if typeIsTotal (getPatternAnnotation pat)
              then [removeAnn pat]
              else generateForType (getPatternAnnotation pat)
          pats -> if isTotal pat then pats else [removeAnn pat] <> pats

      genAs :: [[Pattern ()]]
      genAs = fmap genOrOriginal ([a] <> NE.toList as)

      createTuple :: [Pattern ()] -> Pattern ()
      createTuple items =
        let ne = NE.fromList items
         in PTuple () (NE.head ne) (NE.fromList $ NE.tail ne)
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
  [Pattern ()] ->
  [Pattern ()] ->
  [Pattern ()]
filterMissing patterns required =
  Data.List.nub $ foldr annihiliatePattern required patterns
  where
    annihiliatePattern pat =
      filter
        ( not
            . annihilate pat
        )

removeAnn :: Pattern ann -> Pattern ()
removeAnn p = p $> ()

-- does left pattern satisfy right pattern?
annihilateAll ::
  [(Pattern (), Pattern ())] ->
  Bool
annihilateAll =
  foldr
    (\(a, b) keep -> keep && annihilate a b)
    True

-- | if left is on the right, should we get rid?
annihilate :: Pattern () -> Pattern () -> Bool
annihilate a b | a == b = True
annihilate (PWildcard _) _ = True -- wildcard trumps all
annihilate (PVar _ _) _ = True -- as does var
annihilate (PTuple _ a as) (PTuple _ b bs) =
  let allPairs = zip ([a] <> NE.toList as) ([b] <> NE.toList bs)
   in annihilateAll allPairs
annihilate _ _as = False
