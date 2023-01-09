{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Transform.FlattenPattern (flattenPatterns) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Smol.Core.ExprUtils
import Smol.Core.Helpers (foldMapM)
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Types
import Smol.Core.Types

newtype FlattenState = FlattenState {fsUnique :: Int}

getUnique :: (MonadState FlattenState m) => m Int
getUnique = do
  i <- gets fsUnique
  modify (\st -> st {fsUnique = i + 1})
  pure i

nextVar :: (MonadState FlattenState m) => m Identifier
nextVar = do
  i <- getUnique
  pure $ Identifier $ "v" <> T.pack (show i)

splitByConstructor ::
  ( MonadReader (TCEnv ann) m,
    MonadError (TCError ann) m
  ) =>
  [(Pattern ann, b)] ->
  m (Map Constructor [(ann, [Pattern ann], b)])
splitByConstructor pats = do
  allConstructors <- getAllConstructors (fst <$> pats)
  let getFiltered cons =
        mapMaybe
          ( \case
              (PConstructor ann thisCons args, expr)
                | thisCons == cons -> Just (ann, args, expr)
              (PWildcard ann, expr) ->
                Just (ann, [], expr)
              _ -> Nothing
          )
          pats
  pure $
    foldMap
      (\constructor -> M.singleton constructor (getFiltered constructor))
      allConstructors

getAllConstructors ::
  ( MonadReader (TCEnv ann) m,
    MonadError (TCError ann) m
  ) =>
  [Pattern ann] ->
  m (Set Constructor)
getAllConstructors = foldMapM go
  where
    go (PConstructor _ cons _) = do
      (_, _, otherCons, _) <- lookupConstructor cons
      pure $ S.fromList otherCons
    go _ = pure mempty

flattenPatterns ::
  ( MonadReader (TCEnv ann) m,
    MonadError (TCError ann) m,
    Show ann
  ) =>
  Expr ann ->
  m (Expr ann)
flattenPatterns pats =
  evalStateT (flatten pats) (FlattenState 1)

flatten ::
  ( MonadReader (TCEnv ann) m,
    MonadError (TCError ann) m,
    MonadState FlattenState m,
    Show ann
  ) =>
  Expr ann ->
  m (Expr ann)
flatten (EPatternMatch ann expr innerPats) =
  EPatternMatch ann expr . NE.fromList
    <$> flattenPatternsInternal (NE.toList innerPats)
flatten other = bindExpr flatten other

createNewPatterns ::
  ( MonadState FlattenState m,
    MonadReader (TCEnv ann) m,
    MonadError (TCError ann) m,
    Show ann
  ) =>
  Constructor ->
  [(ann, [Pattern ann], Expr ann)] ->
  m (Pattern ann, Expr ann)
createNewPatterns constructor pats = do
  case NE.nonEmpty pats of
    Nothing -> error $ "no patterns, what the fuck: " <> show pats
    Just nePats ->
      case NE.uncons nePats of
        (neHead, Nothing) -> do
          -- only one pattern in this group, return it unharmed
          let (ann, args, expr) = neHead
          pure (PConstructor ann constructor args, expr)
        _ -> do
          uniqueVar <- nextVar
          let ann = (\(a, _, _) -> a) . NE.head $ nePats
          let newPat = PConstructor ann constructor [PVar ann uniqueVar]
              newExpr newPats =
                flatten (EPatternMatch ann (EVar ann uniqueVar) newPats)
          newPairs <-
            traverse
              ( \(ann', args, expr) -> do
                  let pat = case args of
                        [] -> PWildcard ann'
                        [one] -> one
                        other -> error (show other)
                  pure (pat, expr)
              )
              nePats

          (,) newPat <$> newExpr newPairs

flattenPatternsInternal ::
  ( MonadReader (TCEnv ann) m,
    MonadState FlattenState m,
    Show ann,
    MonadError (TCError ann) m
  ) =>
  [(Pattern ann, Expr ann)] ->
  m [(Pattern ann, Expr ann)]
flattenPatternsInternal pats = do
  byConstructor <- splitByConstructor pats
  -- if there are none, do nowt
  if M.null byConstructor
    then pure pats
    else do
      traverse
        ( uncurry createNewPatterns
        )
        (M.toList byConstructor)
