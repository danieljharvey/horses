{-# LANGUAGE FlexibleContexts #-}

module Smol.Typecheck.Typecheck
  ( typecheck,
  )
where

import Control.Monad.Except
import Smol.Core.Types
import Smol.Typecheck.Elaborate (elaborate)
import Smol.Typecheck.Typeclass.Deduplicate (deduplicateConstraints)
import Smol.Typecheck.Types

-- elaborate an expression, returning it along with constraints,
-- with `\instances -> ...` where there are constraints
typecheck ::
  (MonadError (TCError ann) m, Monoid ann, Show ann, Ord ann) =>
  TCEnv ann ->
  ResolvedExpr ann ->
  m ([Constraint ResolvedDep ann], ResolvedExpr (ResolvedType ann))
typecheck env expr = do
  (typedExpr, typeclassUses) <- elaborate env expr

  -- deduplicate constraints, and match them to the variables that use them
  pure (deduplicateConstraints typeclassUses typedExpr)
