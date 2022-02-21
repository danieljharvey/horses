module Language.Mimsa.Transform.Inliner (inlineInternal, inline, howTrivial, shouldInline) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Language.Mimsa.ExprUtils
import Language.Mimsa.Transform.FindUses
import Language.Mimsa.Transform.Shared
import Language.Mimsa.Types.AST

-- | should we inline this expression?
-- if it's only used in one place, not within a lambda, yes
shouldInline :: Int -> Bool -> Expr var ann -> Bool
shouldInline 1 False _expr = True
shouldInline _ False expr = isJust (howTrivial expr)
shouldInline _ True _ = False

-- | complexity measure of trivial expression
howTrivial :: Expr var ann -> Maybe Int
howTrivial (MyLiteral _ _) = Just 1
howTrivial (MyArray _ as) = (+ 1) . sum <$> traverse howTrivial as
howTrivial (MyRecord _ as) = (+ 1) . sum <$> traverse howTrivial as
howTrivial (MyPair _ a b) = (+ 2) . sum <$> traverse howTrivial [a, b]
howTrivial (MyVar _ _) = Just 1
howTrivial _ = Nothing

-- item that can be inlined, we can add useful info here to help our choices
newtype InlineItem var ann = InlineItem
  { iiExpression :: Expr var ann
  }

-- as we traverse the expression we learn shit
newtype InlineState var ann = InlineState
  { isExpressions :: Map var (InlineItem var ann)
  }

-- static info we can use
data InlineEnv var = InlineEnv {ieUses :: Uses var, ieIsWithinLambda :: Bool}

inlineInternal :: (Ord var) => Expr var ann -> Expr var ann
inlineInternal expr =
  let initialState = InlineState mempty
      initialEnv = InlineEnv (findUses expr) False
   in runReader (evalStateT (inlineExpression expr) initialState) initialEnv

inline :: (Ord var, Eq ann) => Expr var ann -> Expr var ann
inline = repeatUntilEq inlineInternal

storeExpr :: (Ord var) => var -> Expr var ann -> InlineM var ann ()
storeExpr var expr =
  let inlineItem = InlineItem expr
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

type InlineM var ann a =
  StateT (InlineState var ann) (Reader (InlineEnv var)) a

getUsesCount :: (Ord var) => var -> InlineM var ann Int
getUsesCount var = asks (numberOfUses var . ieUses)

substituteVar :: (Ord var) => var -> InlineM var ann (Maybe (Expr var ann))
substituteVar var = do
  maybeExpr <- (fmap . fmap) iiExpression (lookupVar var)
  uses <- getUsesCount var
  inLambda <- asks ieIsWithinLambda
  case maybeExpr of
    Just expr | shouldInline uses inLambda expr -> Just <$> inlineExpression expr
    _ -> pure Nothing

withinLambda :: InlineM var ann a -> InlineM var ann a
withinLambda = local (\ie -> ie {ieIsWithinLambda = True})

inlineExpression :: (Ord var) => Expr var ann -> InlineM var ann (Expr var ann)
inlineExpression (MyLet ann ident expr rest) = do
  storeExpr (nameFromIdent ident) expr
  MyLet ann ident expr <$> inlineExpression rest
inlineExpression (MyVar ann var) = do
  substitute <- substituteVar var
  case substitute of
    Just new -> pure new
    _ -> pure (MyVar ann var)
inlineExpression (MyLambda ann ident body) = do
  body' <- withinLambda (inlineExpression body)
  pure (MyLambda ann ident body')
inlineExpression other =
  bindExpr inlineExpression other
