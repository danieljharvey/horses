{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter2.PatternMatch
  ( interpretPatternMatch,
    interpretLetPattern,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Mimsa.Interpreter2.Monad
import Language.Mimsa.Interpreter2.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError2
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Interpreter.Stack

interpretLetPattern ::
  (Ord var) =>
  InterpretFn var ann ->
  Pattern var (StackFrame var ann) ->
  InterpretExpr var ann ->
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretLetPattern interpretFn pat expr body = do
  -- interpret input
  intExpr <- interpretFn expr
  -- get current stack frame
  currentStackFrame <- getCurrentStackFrame
  -- get new bound variables
  let bindings = fromMaybe [] (patternMatches pat intExpr)
  -- add args to context
  let newStackFrame = foldr (uncurry addVarToFrame) currentStackFrame bindings
  -- run body with closure + new arg
  local (addStackFrame newStackFrame) (interpretFn body)

interpretPatternMatch ::
  (Ord var) =>
  InterpretFn var ann ->
  InterpretExpr var ann ->
  [(Pattern var (StackFrame var ann), InterpretExpr var ann)] ->
  InterpreterM var ann (InterpretExpr var ann)
interpretPatternMatch interpretFn expr' patterns = do
  -- interpret match expression
  intExpr <- interpretFn expr'
  let foldF (pat, patExpr) = case patternMatches pat intExpr of
        Just bindings -> First (Just (patExpr, bindings))
        _ -> First Nothing
  -- get first matching pattern
  case getFirst (foldMap foldF patterns) of
    Just (patExpr, bindings) ->
      do
        -- get current stack frame
        currentStackFrame <- getCurrentStackFrame
        -- add args to context
        let newStackFrame =
              foldr (uncurry addVarToFrame) currentStackFrame bindings
        -- run body with closure + new arg
        local (addStackFrame newStackFrame) (interpretFn patExpr)
    _ ->
      throwError $ PatternMatchFailure expr'

-- pull vars out of expr to match patterns
patternMatches ::
  Pattern var (StackFrame var ann) ->
  InterpretExpr var ann ->
  Maybe [(var, InterpretExpr var ann)]
patternMatches (PWildcard _) _ = pure []
patternMatches (PVar _ name) expr = pure [(name, expr)]
patternMatches (PPair _ pA pB) (MyPair _ a b) = do
  as <- patternMatches pA a
  bs <- patternMatches pB b
  pure $ as <> bs
patternMatches (PRecord _ pAs) (MyRecord _ as)
  | S.null (S.difference (M.keysSet pAs) (M.keysSet as)) = do
    let usefulInputs = M.intersection as pAs
        allPairs = zip (M.elems pAs) (M.elems usefulInputs)
    nice <- traverse (uncurry patternMatches) allPairs
    pure (mconcat nice)
patternMatches (PLit _ pB) (MyLiteral _ b)
  | pB == b = pure mempty
patternMatches (PConstructor _ _pTyCon []) (MyConstructor _ _tyCon) =
  pure mempty
patternMatches (PConstructor _ pTyCon pArgs) (MyApp ann fn val) = do
  (tyCon, args) <- consAppToPattern (MyApp ann fn val)
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

consAppToPattern :: InterpretExpr var ann -> Maybe (TyCon, [InterpretExpr var ann])
consAppToPattern (MyApp _ fn val) = do
  (tyCon, more) <- consAppToPattern fn
  pure (tyCon, more <> [val])
consAppToPattern (MyConstructor _ tyCon) = pure (tyCon, mempty)
consAppToPattern _ = Nothing
