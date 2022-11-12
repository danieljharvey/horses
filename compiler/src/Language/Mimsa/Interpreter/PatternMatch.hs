module Language.Mimsa.Interpreter.PatternMatch
  ( interpretPatternMatch,
    interpretLetPattern,
  )
where

import Control.Monad.Except
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Mimsa.Core
import Language.Mimsa.Interpreter.Monad
import Language.Mimsa.Interpreter.Types
import qualified Language.Mimsa.Types.AST.HOASExpr as HOAS
import Language.Mimsa.Interpreter.ToHOAS
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Typechecker.Unique

interpretLetPattern ::
  InterpretFn ann ->
  InterpretPattern ann ->
    (InterpretExpr ann -> InterpretExpr ann) ->
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretLetPattern interpretFn _pat _expr body = do
  interpretFn body
    {-
  -- interpret input
  _intExpr <- interpretFn expr
  -- get new bound variables
  let _bindings = fromMaybe [] (patternMatches pat intExpr)
  -- run body with closure + new arg
  -- extendStackFrame bindings (interpretFn body)
  interpretFn body -- TODO: we'll change this into a function soon, allow it to be wrong for now
-}

interpretPatternMatch ::
  InterpretFn ann ->
  InterpretExpr ann ->
  [(InterpretPattern ann, InterpretExpr ann -> InterpretExpr ann)] ->
  InterpreterM ann (InterpretExpr ann)
interpretPatternMatch interpretFn expr' patterns = do
  -- interpret match expression
  intExpr <- interpretFn expr'
  let foldF (pat, patExpr) = case patternMatches pat intExpr of
        Just bindings -> First (Just (patExpr, bindings))
        _ -> First Nothing
  -- get first matching pattern
  case getFirst (foldMap foldF patterns) of
    Just (_patExpr, _bindings) ->
      do
        -- run body with closure + new arg
        -- extendStackFrame bindings (interpretFn patExpr)
        --interpretFn patExpr -- TODO: wrong, need to put the stuff in a record and pass it all in
        error "whoa"
    _ ->
      throwError $ PatternMatchFailure (fromHOAS expr')

-- pull vars out of expr to match patterns
patternMatches ::
  InterpretPattern ann ->
  InterpretExpr ann ->
  Maybe [((Name, Unique), InterpretExpr ann)]
patternMatches (PWildcard _) _ = pure []
patternMatches (PVar _ name) expr = pure [(name, expr)]
patternMatches (PTuple _ pA pAs) (HOAS.MyTuple _ a as) = do
  matchA <- patternMatches pA a
  matchAs <-
    traverse
      (uncurry patternMatches)
      (zip (NE.toList pAs) (NE.toList as))
  pure $ matchA <> mconcat matchAs
patternMatches (PRecord _ pAs) (HOAS.MyRecord _ as)
  | S.null (S.difference (M.keysSet pAs) (M.keysSet as)) = do
      let usefulInputs = M.intersection as pAs
          allPairs = zip (M.elems pAs) (M.elems usefulInputs)
      nice <- traverse (uncurry patternMatches) allPairs
      pure (mconcat nice)
patternMatches (PLit _ pB) (HOAS.MyLiteral _ b)
  | pB == b = pure mempty
patternMatches (PConstructor _ _ _pTyCon []) (HOAS.MyConstructor _ _ _tyCon) =
  pure mempty
patternMatches (PConstructor _ _ pTyCon pArgs) (HOAS.MyApp ann fn val) = do
  (tyCon, args) <- consAppToPattern (HOAS.MyApp ann fn val)
  if tyCon /= pTyCon
    then Nothing
    else do
      let allPairs = zip pArgs args
      nice <- traverse (uncurry patternMatches) allPairs
      pure (mconcat nice)
patternMatches (PArray _ pAs NoSpread) (HOAS.MyArray _ as)
  | length pAs == length as = do
      let allPairs = zip pAs as
      nice <- traverse (uncurry patternMatches) allPairs
      pure (mconcat nice)
patternMatches (PArray _ pAs (SpreadWildcard _)) (HOAS.MyArray _ as)
  | length pAs <= length as = do
      let allPairs = zip pAs as
      nice <- traverse (uncurry patternMatches) allPairs
      pure (mconcat nice)
patternMatches (PArray _ pAs (SpreadValue _ a)) (HOAS.MyArray ann as)
  | length pAs <= length as = do
      let binding = (a, HOAS.MyArray ann (drop (length pAs) as))
      let allPairs = zip pAs as
      nice <- traverse (uncurry patternMatches) allPairs
      pure (mconcat nice <> [binding])
patternMatches (PString _ pA pAs) (HOAS.MyLiteral _ (MyString (StringType str))) | not (T.null str) =
  do
    let bindingA = case pA of
          (StrValue ann a) ->
            [ ( a,
                HOAS.MyLiteral
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
                HOAS.MyLiteral
                  ann
                  ( MyString (StringType (T.drop 1 str))
                  )
              )
            ]
          _ -> []
    pure (bindingA <> bindingAs)
patternMatches _ _ = Nothing

consAppToPattern ::
  InterpretExpr ann ->
  Maybe (TyCon, [InterpretExpr ann])
consAppToPattern (HOAS.MyApp _ fn val) = do
  (tyCon, more) <- consAppToPattern fn
  pure (tyCon, more <> [val])
consAppToPattern (HOAS.MyConstructor _ _ tyCon) = pure (tyCon, mempty)
consAppToPattern _ = Nothing
