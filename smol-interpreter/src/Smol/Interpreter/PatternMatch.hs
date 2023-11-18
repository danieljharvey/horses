module Smol.Interpreter.PatternMatch
  ( interpretPatternMatch,
  )
where

import Control.Monad.Except
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.Monoid
import qualified Data.Sequence as Seq
import Smol.Interpreter.Monad
import Smol.Interpreter.Types
import Smol.Interpreter.Types.InterpreterError
import Smol.Core.Types.Constructor
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.Pattern
import Smol.Core.Types.ResolvedDep
import Smol.Core.Types.Spread

interpretPatternMatch ::
  InterpretFn ann ->
  InterpretExpr ann ->
  [(InterpretPattern ann, InterpretExpr ann)] ->
  InterpreterM ann (InterpretExpr ann)
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
        -- run body with closure + new arg
        extendStackFrame bindings (interpretFn patExpr)
    _ ->
      throwError $ PatternMatchFailure expr'

-- pull vars out of expr to match patterns
patternMatches ::
  InterpretPattern ann ->
  InterpretExpr ann ->
  Maybe [(ResolvedDep Identifier, InterpretExpr ann)]
patternMatches (PWildcard _) _ = pure []
patternMatches (PVar _ name) expr = pure [(name, expr)]
patternMatches (PTuple _ pA pAs) (ETuple _ a as) = do
  matchA <- patternMatches pA a
  matchAs <-
    traverse
      (uncurry patternMatches)
      (zip (NE.toList pAs) (NE.toList as))
  pure $ matchA <> mconcat matchAs
patternMatches (PLiteral _ pB) (EPrim _ b)
  | pB == b = pure mempty
patternMatches (PConstructor _ pTyCon []) (EConstructor _ tyCon) | pTyCon == tyCon = do
  pure mempty
patternMatches (PConstructor _ pTyCon pArgs) (EApp ann fn val) = do
  (tyCon, args) <- consAppToPattern (EApp ann fn val)
  if tyCon /= pTyCon
    then Nothing
    else do
      let allPairs = zip pArgs args
      nice <- traverse (uncurry patternMatches) allPairs
      pure (mconcat nice)
patternMatches (PArray _ pAs NoSpread) (EArray _ as)
  | length pAs == length as = do
      let allPairs = zip pAs (toList as)
      nice <- traverse (uncurry patternMatches) allPairs
      pure (mconcat nice)
patternMatches (PArray _ pAs (SpreadWildcard _)) (EArray _ as)
  | length pAs <= length as = do
      let allPairs = zip pAs (toList as)
      nice <- traverse (uncurry patternMatches) allPairs
      pure (mconcat nice)
patternMatches (PArray _ pAs (SpreadValue _ a)) (EArray ann as)
  | length pAs <= length as = do
      let binding = (a, EArray ann (Seq.fromList $ drop (length pAs) (toList as)))
      let allPairs = zip pAs (toList as)
      nice <- traverse (uncurry patternMatches) allPairs
      pure (mconcat nice <> [binding])
patternMatches _ _ = Nothing

consAppToPattern :: InterpretExpr ann -> Maybe (ResolvedDep Constructor, [InterpretExpr ann])
consAppToPattern (EApp _ fn val) = do
  (tyCon, more) <- consAppToPattern fn
  pure (tyCon, more <> [val])
consAppToPattern (EConstructor _ tyCon) = pure (tyCon, mempty)
consAppToPattern _ = Nothing
