
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Smol.Core.Typecheck.Typecheck
  ( typecheck
  )
where

import Control.Monad.Except
import Smol.Core.Typecheck.Types
import Smol.Core.Types
import Smol.Core.Typecheck.Elaborate (elaborate)
import Smol.Core.Typecheck.Typeclass (convertExprToUseTypeclassDictionary)

-- elaborate an expression, returning it along with constraints,
-- with `\instances -> ...` where there are constraints
typecheck :: (MonadError (TCError ann) m, Monoid ann, Show ann, Ord ann) =>
    TCEnv ann -> ResolvedExpr ann -> m ([Constraint ann], ResolvedExpr (ResolvedType ann))
typecheck env expr = do
  (typedExpr, typeclassUses) <- elaborate env expr

  -- inline those functions
  convertExprToUseTypeclassDictionary env typeclassUses typedExpr

