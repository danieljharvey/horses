{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Transform.InlineDeps
  ( inlineStoreExpression,
  )
where

import Control.Monad.State
import Data.Foldable
import qualified Data.Map as M
import Language.Mimsa.Transform.Inliner
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Scope

-- When inlining a StoreExpression we need to do the following:
-- 1. Take it's dependencies and store them as potential items for inlining
-- 1.5 Also add your deps's deps to our deps
-- 2. Run inlining
-- 3. Hope that maybe the deps fall away in another step?
inlineStoreExpression :: ResolvedExpression ann -> Expr Variable ann
inlineStoreExpression resolvedExpr =
  let withDepsM =
        traverse_
          (uncurry storeExprInState)
          (M.toList (getScope $ reScope resolvedExpr))
      initialState = execState withDepsM (InlineState mempty)
   in inlineInternal initialState (reVarExpression resolvedExpr)
