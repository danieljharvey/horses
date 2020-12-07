module Language.Mimsa.Interpreter.HighestVar (highestVar) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Semigroup (Max (..))
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

-- interpreter starts creating new variables, so we need to check the ones we
-- have to avoid collisions
highestVar :: Expr Variable ann -> Int
highestVar expr' = max 0 $ getMax $ withMonoid getHighest expr'
  where
    getHighest (MyVar _ (NumberedVar i)) = Max i
    getHighest _ = mempty

-- is this just shitty foldable?
withMonoid ::
  (Monoid m) =>
  (Expr Variable ann -> m) ->
  Expr Variable ann ->
  m
withMonoid f whole@(MyLiteral _ _) = f whole
withMonoid f whole@(MyVar _ _) = f whole
withMonoid f whole@(MyLet _ _ bindExpr' inExpr) =
  f whole
    <> withMonoid f bindExpr'
    <> withMonoid f inExpr
withMonoid f whole@(MyLetPair _ _binderA _binderB bindExpr' inExpr) =
  f whole
    <> withMonoid f bindExpr'
    <> withMonoid f inExpr
withMonoid f whole@(MyInfix _ _ a b) =
  f whole
    <> withMonoid f a
    <> withMonoid f b
withMonoid f whole@(MyLambda _ _binder expr) =
  f whole
    <> withMonoid f expr
withMonoid f whole@(MyApp _ func arg) =
  f whole
    <> withMonoid f func
    <> withMonoid f arg
withMonoid f whole@(MyIf _ matchExpr thenExpr elseExpr) =
  f whole
    <> withMonoid f matchExpr
    <> withMonoid f thenExpr
    <> withMonoid f elseExpr
withMonoid f whole@(MyPair _ a b) =
  f whole
    <> withMonoid f a
    <> withMonoid f b
withMonoid f whole@(MyRecord _ items) =
  f whole
    <> mconcat
      ( snd <$> M.toList (withMonoid f <$> items)
      )
withMonoid f whole@(MyRecordAccess _ expr _name) =
  f whole <> withMonoid f expr
withMonoid f whole@(MyData _ _ expr) =
  f whole <> withMonoid f expr
withMonoid f whole@(MyConstructor _ _) = f whole
withMonoid f whole@(MyConsApp _ func arg) =
  f whole
    <> withMonoid f func
    <> withMonoid f arg
withMonoid f whole@(MyCaseMatch _ matchExpr caseExprs catchExpr) =
  f whole
    <> withMonoid f matchExpr
    <> mconcat
      ( NE.toList
          (withMonoid f <$> (snd <$> caseExprs))
      )
    <> maybe mempty (withMonoid f) catchExpr
