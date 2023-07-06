{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Calc.Interpreter
  ( runInterpreter,
    interpret,
    interpretModule,
    InterpreterError (..),
    InterpreterState (..),
    InterpreterEnv (..),
  )
where

import Calc.Types
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Coerce
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid (First (..))

-- | type for interpreter state
newtype InterpreterState ann = InterpreterState
  { isFunctions :: Map FunctionName (Function ann)
  }

-- | type of errors that can occur
data InterpreterError ann
  = NonBooleanPredicate ann (Expr ann)
  | FunctionNotFound FunctionName [FunctionName]
  | VarNotFound Identifier [Identifier]
  | NoPatternsMatched (Expr ann) (NE.NonEmpty (Pattern ann))
  deriving stock (Eq, Ord, Show)

-- | type of Reader env for interpreter state
-- we use this for scoped temporary state
newtype InterpreterEnv ann = InterpreterEnv
  { ieVars :: Map Identifier (Expr ann)
  }

newtype InterpretM ann a = InterpretM {runInterpretM :: ReaderT (InterpreterEnv ann) (StateT (InterpreterState ann) (Either (InterpreterError ann))) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError (InterpreterError ann),
      MonadState (InterpreterState ann),
      MonadReader (InterpreterEnv ann)
    )

runInterpreter ::
  InterpretM ann a ->
  Either (InterpreterError ann) a
runInterpreter = flip evalStateT initialState . flip runReaderT initialEnv . runInterpretM
  where
    initialEnv = InterpreterEnv mempty
    initialState = InterpreterState mempty

-- | run an `InterpretM` action, after adding some arguments into the
-- Reader environment
-- we use the Reader env here because the vars disappear after we use them,
-- say, in a function
withVars ::
  [ArgumentName] ->
  [Expr ann] ->
  InterpretM ann a ->
  InterpretM ann a
withVars fnArgs inputs =
  let newVars = M.fromList $ zip (coerce <$> fnArgs) inputs
   in local
        ( \(InterpreterEnv ieVars) ->
            InterpreterEnv $ ieVars <> newVars
        )

-- | lookup a variable in the Reader environment
lookupVar :: Identifier -> InterpretM ann (Expr ann)
lookupVar identifier = do
  maybeValue <- asks (M.lookup identifier . ieVars)
  case maybeValue of
    Just expr -> pure expr
    Nothing -> do
      allVars <- asks (M.keys . ieVars)
      throwError (VarNotFound identifier allVars)

interpretInfix ::
  ann ->
  Op ->
  Expr ann ->
  Expr ann ->
  InterpretM ann (Expr ann)
interpretInfix ann OpAdd (EPrim _ (PInt a)) (EPrim _ (PInt b)) =
  pure $ EPrim ann (PInt $ a + b)
interpretInfix ann OpSubtract (EPrim _ (PInt a)) (EPrim _ (PInt b)) =
  pure $ EPrim ann (PInt $ a - b)
interpretInfix ann OpMultiply (EPrim _ (PInt a)) (EPrim _ (PInt b)) =
  pure $ EPrim ann (PInt $ a * b)
interpretInfix ann OpEquals (EPrim _ a) (EPrim _ b) =
  pure $ EPrim ann (PBool $ a == b)
interpretInfix ann op a b = do
  iA <- interpret a
  iB <- interpret b
  interpretInfix ann op iA iB

-- | look up the function, adds the arguments into the Reader environment
-- then interpret the function body
interpretApply :: FunctionName -> [Expr ann] -> InterpretM ann (Expr ann)
interpretApply fnName args = do
  fn <- gets (M.lookup fnName . isFunctions)
  case fn of
    Just (Function {fnArgs, fnBody}) ->
      withVars (fst <$> fnArgs) args (interpret fnBody)
    Nothing -> do
      allFnNames <- gets (M.keys . isFunctions)
      throwError (FunctionNotFound fnName allFnNames)

-- | just keep reducing the thing until the smallest thing
interpret ::
  Expr ann ->
  InterpretM ann (Expr ann)
interpret (EPrim ann p) =
  pure (EPrim ann p)
interpret (EVar _ ident) =
  lookupVar ident
interpret (EApply _ fnName args) =
  interpretApply fnName args
interpret (EInfix ann op a b) =
  interpretInfix ann op a b
interpret (ETuple ann a as) = do
  aA <- interpret a
  asA <- traverse interpret as
  pure (ETuple ann aA asA)
interpret (EPatternMatch _ expr pats) = do
  exprA <- interpret expr
  interpretPatternMatch exprA pats
interpret (EIf ann predExpr thenExpr elseExpr) = do
  predA <- interpret predExpr
  case predA of
    (EPrim _ (PBool True)) -> interpret thenExpr
    (EPrim _ (PBool False)) -> interpret elseExpr
    other -> throwError (NonBooleanPredicate ann other)

interpretPatternMatch ::
  Expr ann ->
  NE.NonEmpty (Pattern ann, Expr ann) ->
  InterpretM ann (Expr ann)
interpretPatternMatch expr' patterns = do
  -- interpret match expression
  intExpr <- interpret expr'
  let foldF (pat, patExpr) = case patternMatches pat intExpr of
        Just bindings -> First (Just (patExpr, bindings))
        _ -> First Nothing

  -- get first matching pattern
  case getFirst (foldMap foldF patterns) of
    Just (patExpr, bindings) ->
      let vars = fmap (coerce . fst) bindings
          exprs = fmap snd bindings
       in withVars vars exprs (interpret patExpr)
    _ -> throwError (NoPatternsMatched expr' (fst <$> patterns))

-- pull vars out of expr to match patterns
patternMatches ::
  Pattern ann ->
  Expr ann ->
  Maybe [(Identifier, Expr ann)]
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
patternMatches _ _ = Nothing

interpretModule ::
  Module ann ->
  InterpretM ann (Expr ann)
interpretModule (Module {mdExpr, mdFunctions}) = do
  let fnMap = M.fromList $ (\fn -> (fnFunctionName fn, fn)) <$> mdFunctions
  put (InterpreterState fnMap)
  interpret mdExpr
