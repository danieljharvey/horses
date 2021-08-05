{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Typechecker.TcMonad where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (MonadState, State, gets, modify, runState)
import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

type TcMonad = ExceptT TypeError (ReaderT Swaps (State TypecheckState))

data TypecheckState = TypecheckState
  { tcsNum :: Int,
    tcsTypedHoles :: Map Name (Annotation, Int)
  }

runTcMonad ::
  Swaps ->
  TcMonad a ->
  Either TypeError a
runTcMonad swaps value =
  fst either'
  where
    defaultState =
      TypecheckState 0 mempty
    either' =
      runState
        (runReaderT (runExceptT value) swaps)
        defaultState

getNextUniVar :: (MonadState TypecheckState m) => m Int
getNextUniVar = do
  nextUniVar <- gets tcsNum
  modify (\s -> s {tcsNum = nextUniVar + 1})
  pure nextUniVar

-- | Get a new unknown for a typed hole and return it's monotype
addTypedHole ::
  (MonadState TypecheckState m) =>
  Annotation ->
  Name ->
  m MonoType
addTypedHole ann name = do
  i <- getNextUniVar
  modify (\s -> s {tcsTypedHoles = tcsTypedHoles s <> M.singleton name (ann, i)})
  pure $ MTVar ann (TVNum i)

-- todo - look up index in substitutions to get type
getTypedHoles :: (MonadState TypecheckState m) => Substitutions -> m (Map Name MonoType)
getTypedHoles (Substitutions subs) = do
  holes <- gets tcsTypedHoles
  let getMonoType = \(ann, i) -> case M.lookup (TVNum i) subs of
        Just a -> a
        Nothing -> MTVar ann (TVNum i)
  pure $ fmap getMonoType holes

getUnknown :: (MonadState TypecheckState m) => Annotation -> m MonoType
getUnknown ann = MTVar ann . TVNum <$> getNextUniVar

variableToTypeIdentifier :: Variable -> TypeIdentifier
variableToTypeIdentifier (NamedVar n) = TVName (coerce n)
variableToTypeIdentifier (NumberedVar i) = TVNum i
