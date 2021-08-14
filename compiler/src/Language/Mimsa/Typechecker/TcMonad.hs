{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Typechecker.TcMonad
  ( defaultTcState,
    getUnknown,
    addTypedHole,
    getTypedHoles,
    TypecheckState (..),
    variableToTypeIdentifier,
  )
where

import Control.Monad.State (MonadState, gets, modify)
import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

data TypecheckState = TypecheckState
  { tcsNum :: Int,
    tcsTypedHoles :: Map Name (Annotation, Int)
  }

defaultTcState :: TypecheckState
defaultTcState = TypecheckState 0 mempty

getNextUniVar :: (MonadState TypecheckState m) => m Int
getNextUniVar = do
  nextUniVar <- gets tcsNum
  modify (\s -> s {tcsNum = nextUniVar + 1})
  pure nextUniVar

typeFromUniVar :: Annotation -> Int -> MonoType
typeFromUniVar ann = MTVar ann . TVNum

getUnknown :: (MonadState TypecheckState m) => Annotation -> m MonoType
getUnknown ann = typeFromUniVar ann <$> getNextUniVar

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
getTypedHoles ::
  (MonadState TypecheckState m) =>
  Substitutions ->
  m (Map Name MonoType)
getTypedHoles (Substitutions subs) = do
  holes <- gets tcsTypedHoles
  let getMonoType = \(ann, i) -> case M.lookup (TVNum i) subs of
        Just a -> a
        Nothing -> MTVar ann (TVNum i)
  pure $ fmap getMonoType holes

variableToTypeIdentifier :: Variable -> TypeIdentifier
variableToTypeIdentifier (NamedVar n) = TVName (coerce n)
variableToTypeIdentifier (NumberedVar i) = TVNum i
