{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Transform.Inliner
  ( inlineInternal,
    InlineState (..),
    inline,
    storeExprInState,
    howTrivial,
    shouldInline,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Smol.Core.ExprUtils
import Smol.Core.FindUses
import Smol.Core.Types

type InlineM dep ann a =
  StateT (InlineState dep ann) (Reader (InlineEnv dep)) a

data IsRecursive = Recursive | NotRecursive

data IsWithinLambda = WithinLambda | NotWithinLambda

newtype VarUses = VarUses Int

-- item that can be inlined, we can add useful info here to help our choices
data InlineItem dep ann = InlineItem
  { iiExpression :: Expr dep ann,
    _iiRecursive :: IsRecursive
  }

-- as we traverse the expression we learn shit
newtype InlineState dep ann = InlineState
  { isExpressions :: Map (dep Identifier) (InlineItem dep ann)
  }

-- static info we can use
data InlineEnv var = InlineEnv
  { ieUses :: Uses var,
    ieIsWithinLambda :: IsWithinLambda
  }

-- | should we inline this expression?
-- if it's only used in one place, not within a lambda, yes
shouldInline :: VarUses -> IsWithinLambda -> InlineItem dep ann -> Bool
-- item is recursive, never inline it
shouldInline _ _ (InlineItem _ Recursive) = False
-- item is used once, always inline it
shouldInline (VarUses 1) NotWithinLambda _expr = True
-- item is not within a lambda, and is trivial, move it
shouldInline _ NotWithinLambda (InlineItem expr _) = isJust (howTrivial expr)
shouldInline _ WithinLambda _ = False

-- | complexity measure of trivial expression
howTrivial :: Expr dep ann -> Maybe Int
howTrivial (EPrim _ _) = Just 1
howTrivial (EArray _ as) = (+ 1) . sum <$> traverse howTrivial as
howTrivial (ERecord _ as) = (+ 1) . sum <$> traverse howTrivial as
howTrivial (ETuple _ a as) = (+ 2) . sum <$> traverse howTrivial ([a] <> NE.toList as)
howTrivial EVar {} = Just 1
howTrivial _ = Nothing

inlineInternal ::
  (Ord (dep Identifier)) =>
  InlineState dep ann ->
  Expr dep ann ->
  Expr dep ann
inlineInternal initialState expr =
  let initialEnv = InlineEnv (findUses expr) NotWithinLambda
   in runReader (evalStateT (inlineExpression expr) initialState) initialEnv

inline :: (Ord (dep Identifier)) => Expr dep ann -> Expr dep ann
inline = inlineInternal (InlineState mempty)

storeExprInState ::
  (Ord (dep Identifier), MonadState (InlineState dep ann) m) =>
  dep Identifier ->
  Expr dep ann ->
  m ()
storeExprInState var expr =
  let isRecursive = memberInUses var (findUses expr)
      inlineItem = InlineItem expr (if isRecursive then Recursive else NotRecursive)
   in modify
        ( \s ->
            s
              { isExpressions =
                  isExpressions s
                    <> M.singleton var inlineItem
              }
        )

lookupVar ::
  (Ord (dep Identifier)) =>
  dep Identifier ->
  InlineM dep ann (Maybe (InlineItem dep ann))
lookupVar var = do
  gets (M.lookup var . isExpressions)

getUsesCount ::
  (Ord (dep Identifier)) =>
  dep Identifier ->
  InlineM dep ann VarUses
getUsesCount var = do
  i <- asks (numberOfUses var . ieUses)
  pure (VarUses i)

substituteVar ::
  (Ord (dep Identifier)) =>
  dep Identifier ->
  InlineM dep ann (Maybe (Expr dep ann))
substituteVar var = do
  maybeItem <- lookupVar var
  uses <- getUsesCount var
  inLambda <- asks ieIsWithinLambda
  case maybeItem of
    Just item
      | shouldInline uses inLambda item ->
          pure $ Just (iiExpression item)
    _ -> pure Nothing

withinLambda :: InlineM dep ann a -> InlineM dep ann a
withinLambda = local (\ie -> ie {ieIsWithinLambda = WithinLambda})

inlineExpression ::
  (Ord (dep Identifier)) =>
  Expr dep ann ->
  InlineM dep ann (Expr dep ann)
inlineExpression (ELet ann ident expr rest) = do
  storeExprInState ident expr
  ELet ann ident <$> inlineExpression expr <*> inlineExpression rest
inlineExpression (EVar ann var) = do
  substitute <- substituteVar var
  case substitute of
    Just new -> pure new
    _ -> pure (EVar ann var)
inlineExpression (ELambda ann ident body) = do
  body' <- withinLambda (inlineExpression body)
  pure (ELambda ann ident body')
inlineExpression other =
  bindExpr inlineExpression other
