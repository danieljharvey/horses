{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Transform.Inliner
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Language.Mimsa.ExprUtils
import Language.Mimsa.Transform.FindUses
import Language.Mimsa.Transform.Shared
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Modules.ModuleName

type InlineM var ann a =
  StateT (InlineState var ann) (Reader (InlineEnv var)) a

data IsRecursive = Recursive | NotRecursive

data IsWithinLambda = WithinLambda | NotWithinLambda

newtype VarUses = VarUses Int

-- item that can be inlined, we can add useful info here to help our choices
data InlineItem var ann = InlineItem
  { iiExpression :: Expr var ann,
    _iiRecursive :: IsRecursive
  }

-- as we traverse the expression we learn shit
newtype InlineState var ann = InlineState
  { isExpressions :: Map var (InlineItem var ann)
  }

-- static info we can use
data InlineEnv var = InlineEnv
  { ieUses :: Uses var,
    ieIsWithinLambda :: IsWithinLambda
  }

-- | should we inline this expression?
-- if it's only used in one place, not within a lambda, yes
shouldInline :: VarUses -> IsWithinLambda -> InlineItem var ann -> Bool
-- item is recursive, never inline it
shouldInline _ _ (InlineItem _ Recursive) = False
-- item is used once, always inline it
shouldInline (VarUses 1) NotWithinLambda _expr = True
-- item is not within a lambda, and is trivial, move it
shouldInline _ NotWithinLambda (InlineItem expr _) = isJust (howTrivial expr)
shouldInline _ WithinLambda _ = False

-- | complexity measure of trivial expression
howTrivial :: Expr var ann -> Maybe Int
howTrivial (MyLiteral _ _) = Just 1
howTrivial (MyArray _ as) = (+ 1) . sum <$> traverse howTrivial as
howTrivial (MyRecord _ as) = (+ 1) . sum <$> traverse howTrivial as
howTrivial (MyPair _ a b) = (+ 2) . sum <$> traverse howTrivial [a, b]
howTrivial MyVar {} = Just 1
howTrivial _ = Nothing

inlineInternal :: (Ord var) => InlineState var ann -> Expr var ann -> Expr var ann
inlineInternal initialState expr =
  let initialEnv = InlineEnv (findUses expr) NotWithinLambda
   in runReader (evalStateT (inlineExpression expr) initialState) initialEnv

inline :: (Ord var, Eq ann) => Expr var ann -> Expr var ann
inline = repeatUntilEq (inlineInternal (InlineState mempty))

storeExprInState ::
  (Ord var, MonadState (InlineState var ann) m) =>
  var ->
  Maybe ModuleName ->
  Expr var ann ->
  m ()
storeExprInState var modName expr =
  let isRecursive = memberInUses var modName (findUses expr)
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
  (Ord var) =>
  var ->
  InlineM var ann (Maybe (InlineItem var ann))
lookupVar var = do
  gets (M.lookup var . isExpressions)

getUsesCount :: (Ord var) => var -> Maybe ModuleName -> InlineM var ann VarUses
getUsesCount var modName = do
  i <- asks (numberOfUses var modName . ieUses)
  pure (VarUses i)

substituteVar :: (Ord var) => var -> Maybe ModuleName -> InlineM var ann (Maybe (Expr var ann))
substituteVar var modName = do
  maybeItem <- lookupVar var
  uses <- getUsesCount var modName
  inLambda <- asks ieIsWithinLambda
  case maybeItem of
    Just item
      | shouldInline uses inLambda item ->
          pure $ Just (iiExpression item)
    _ -> pure Nothing

withinLambda :: InlineM var ann a -> InlineM var ann a
withinLambda = local (\ie -> ie {ieIsWithinLambda = WithinLambda})

inlineExpression :: (Ord var) => Expr var ann -> InlineM var ann (Expr var ann)
inlineExpression (MyLet ann ident expr rest) = do
  storeExprInState (nameFromIdent ident) Nothing expr
  MyLet ann ident <$> inlineExpression expr <*> inlineExpression rest
inlineExpression (MyVar ann modName var) = do
  substitute <- substituteVar var modName
  case substitute of
    Just new -> pure new
    _ -> pure (MyVar ann modName var)
inlineExpression (MyLambda ann ident body) = do
  body' <- withinLambda (inlineExpression body)
  pure (MyLambda ann ident body')
inlineExpression other =
  bindExpr inlineExpression other
