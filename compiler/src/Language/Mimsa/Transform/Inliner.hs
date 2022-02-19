module Language.Mimsa.Transform.Inliner (inline, shouldInline) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.ExprUtils
import Language.Mimsa.Transform.FindUses
import Language.Mimsa.Types.AST

maximumScore :: Int
maximumScore = 4

shouldInline :: Int -> Expr var ann -> Bool
shouldInline i (MyLiteral _ _) = i < maximumScore
shouldInline i (MyArray _ as) = and (shouldInline i <$> as)
shouldInline i (MyRecord _ as) = and (shouldInline i <$> as)
shouldInline _ _ = False

-- item that can be inlined, we can add useful info here to help our choices
newtype InlineItem var ann = InlineItem
  { iiExpression :: Expr var ann
  }

-- as we traverse the expression we learn shit
newtype InlineState var ann = InlineState
  { isExpressions :: Map var (InlineItem var ann)
  }

-- static info we can use
newtype InlineEnv var = InlineEnv {ieUses :: Uses var}

inline :: (Ord var) => Expr var ann -> Expr var ann
inline expr =
  let initialState = InlineState mempty
      initialEnv = InlineEnv (findUses expr)
   in runReader (evalStateT (inlineExpression expr) initialState) initialEnv

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
  case maybeExpr of
    Just expr | shouldInline uses expr -> pure (Just expr)
    _ -> pure Nothing

inlineExpression :: (Ord var) => Expr var ann -> InlineM var ann (Expr var ann)
inlineExpression (MyLet ann ident expr rest) = do
  storeExpr (nameFromIdent ident) expr
  MyLet ann ident expr <$> inlineExpression rest
inlineExpression (MyVar ann var) = do
  substitute <- substituteVar var
  case substitute of
    Just new -> pure new
    _ -> pure (MyVar ann var)
inlineExpression other =
  bindExpr inlineExpression other
