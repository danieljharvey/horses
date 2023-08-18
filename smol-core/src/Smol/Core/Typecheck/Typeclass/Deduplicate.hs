{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Typecheck.Typeclass.Deduplicate
  ( deduplicateConstraints,
    findDedupedConstraints,
    identForConstraint,
  )
where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Smol.Core.ExprUtils
import Smol.Core.Typecheck.Typeclass.Helpers (isConcrete)
import Smol.Core.Typecheck.Types
import Smol.Core.Types

-- | find deduplicated constraints and apply them to expr
deduplicateConstraints ::
  (Ord ann) =>
  M.Map (ResolvedDep Identifier) (Constraint ann) ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  ([Constraint ann], Expr ResolvedDep (Type ResolvedDep ann))
deduplicateConstraints constraints expr = do
  let (dedupedConstraints, nameSwaps) = findDedupedConstraints constraints
   in (dedupedConstraints, swapExprVarnames nameSwaps expr)

identForConstraint :: Integer -> ResolvedDep Identifier
identForConstraint = TypeclassCall "valuefromdictionary" . fromIntegral

-- just because we use a method twice doesn't mean we want to pass it in twice
-- returns a new ordered set of constraints with fresh names,
-- and a list of substitutions to change in the expression to make everything
-- work
findDedupedConstraints ::
  (Ord ann) =>
  M.Map (ResolvedDep Identifier) (Constraint ann) ->
  ([Constraint ann], M.Map (ResolvedDep Identifier) (ResolvedDep Identifier))
findDedupedConstraints dupes =
  let initial = (mempty, mempty, 0)
      deduped =
        foldl'
          ( \(found, swaps, count) (ident, constraint) ->
              if isConcrete constraint
                then (found, swaps, count)
                else case M.lookup constraint found of
                  Just foundIdent ->
                    ( found,
                      swaps <> M.singleton ident foundIdent,
                      count
                    )
                  Nothing ->
                    let newCount = count + 1
                        newIdent = identForConstraint count
                     in ( found <> M.singleton constraint newIdent,
                          swaps <> M.singleton ident newIdent,
                          newCount
                        )
          )
          initial
          (M.toList dupes)
      (finalFound, finalSwaps, _) = deduped
   in (fst <$> M.toList finalFound, finalSwaps)

-- | swap var names of typeclass calls for their new deduped ones
swapExprVarnames ::
  M.Map (ResolvedDep Identifier) (ResolvedDep Identifier) ->
  Expr ResolvedDep ann ->
  Expr ResolvedDep ann
swapExprVarnames swappies =
  go
  where
    newIdent ident = fromMaybe ident (M.lookup ident swappies)

    go (EVar ann ident) =
      EVar ann (newIdent ident)
    go (ELambda ann ident body) =
      ELambda ann (newIdent ident) (go body)
    go other = mapExpr go other
