{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.PatternMatch
  ( patternMatch,
  )
where

import Control.Monad.Except
import Data.Foldable
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers

patternMatch ::
  (Monoid ann) =>
  Expr Variable ann ->
  [(Pattern Variable ann, Expr Variable ann)] ->
  App ann (Expr Variable ann)
patternMatch expr' patterns = do
  let foldF (pat, patExpr) = case patternMatches pat expr' of
        Just bindings -> First (Just (patExpr, bindings))
        _ -> First Nothing
  case getFirst (foldMap foldF patterns) of
    Just (patExpr, bindings) -> pure $ createMatchExpression bindings patExpr
    _ ->
      throwError $ PatternMatchFailure expr'

-- pull vars out of expr to match patterns
patternMatches ::
  Pattern Variable ann ->
  Expr Variable ann ->
  Maybe [(Variable, Expr Variable ann)]
patternMatches (PWildcard _) _ = pure []
patternMatches (PVar _ name) expr = pure [(name, expr)]
patternMatches (PPair _ pA pB) (MyPair _ a b) = do
  as <- patternMatches pA a
  bs <- patternMatches pB b
  pure $ as <> bs
patternMatches (PRecord _ pAs) (MyRecord _ as)
  | S.null (S.difference (M.keysSet pAs) (M.keysSet as)) = do
    let allPairs = zip (M.elems pAs) (M.elems as)
    nice <- traverse (uncurry patternMatches) allPairs
    pure (mconcat nice)
patternMatches (PLit _ pB) (MyLiteral _ b)
  | pB == b = pure mempty
patternMatches (PConstructor _ _pTyCon []) (MyConstructor _ _tyCon) =
  pure mempty
patternMatches (PConstructor _ pTyCon pArgs) (MyConsApp ann fn val) = do
  (tyCon, args) <- consAppToPattern (MyConsApp ann fn val)
  if tyCon /= pTyCon
    then Nothing
    else do
      let allPairs = zip pArgs args
      nice <- traverse (uncurry patternMatches) allPairs
      pure (mconcat nice)
patternMatches (PArray _ pAs NoSpread) (MyArray _ as)
  | length pAs == length as = do
    let allPairs = zip pAs as
    nice <- traverse (uncurry patternMatches) allPairs
    pure (mconcat nice)
patternMatches (PArray _ pAs (SpreadWildcard _)) (MyArray _ as)
  | length pAs <= length as = do
    let allPairs = zip pAs as
    nice <- traverse (uncurry patternMatches) allPairs
    pure (mconcat nice)
patternMatches (PArray _ pAs (SpreadValue _ a)) (MyArray ann as)
  | length pAs <= length as = do
    let binding = (a, MyArray ann (drop (length pAs) as))
    let allPairs = zip pAs as
    nice <- traverse (uncurry patternMatches) allPairs
    pure (mconcat nice <> [binding])
patternMatches (PString _ pA pAs) (MyLiteral _ (MyString (StringType str))) | not (T.null str) =
  do
    let bindingA = case pA of
          (StrValue ann a) ->
            [ ( a,
                MyLiteral
                  ann
                  ( MyString
                      ( StringType (T.singleton (T.head str))
                      )
                  )
              )
            ]
          _ -> []
        bindingAs = case pAs of
          (StrValue ann as) ->
            [ ( as,
                MyLiteral
                  ann
                  ( MyString (StringType (T.drop 1 str))
                  )
              )
            ]
          _ -> []
    pure (bindingA <> bindingAs)
patternMatches _ _ = Nothing

consAppToPattern :: Expr Variable ann -> Maybe (TyCon, [Expr Variable ann])
consAppToPattern (MyConsApp _ fn val) = do
  (tyCon, more) <- consAppToPattern fn
  pure (tyCon, more <> [val])
consAppToPattern (MyConstructor _ tyCon) = pure (tyCon, mempty)
consAppToPattern _ = Nothing

-- apply each part of the constructor to the output function
createMatchExpression ::
  (Monoid ann) =>
  [(Variable, Expr Variable ann)] ->
  Expr Variable ann ->
  Expr Variable ann
createMatchExpression bindings a =
  foldl' (\rest (binder, var) -> MyLet mempty binder var rest) a bindings
