module Language.Mimsa.Typechecker.TcMonad where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (State, gets, modify, runState)
import qualified Data.Map as M
import Data.Map (Map)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

type TcMonad = ExceptT TypeError (ReaderT Swaps (State TypecheckState))

data TypecheckState
  = TypecheckState
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
      TypecheckState 1 mempty
    either' =
      runState
        (runReaderT (runExceptT value) swaps)
        defaultState

getNextUniVar :: TcMonad Int
getNextUniVar = do
  nextUniVar <- gets tcsNum
  modify (\s -> s {tcsNum = nextUniVar + 1})
  pure nextUniVar

-- | Get a new unknown for a typed hole and return it's monotype
addTypedHole :: Annotation -> Name -> TcMonad MonoType
addTypedHole ann name = do
  i <- getNextUniVar
  modify (\s -> s {tcsTypedHoles = tcsTypedHoles s <> M.singleton name (ann, i)})
  pure $ MTVar ann (NumberedVar i)

-- todo - look up index in substitutions to get type
getTypedHoles :: Substitutions -> TcMonad (Map Name MonoType)
getTypedHoles (Substitutions subs) = do
  holes <- gets tcsTypedHoles
  let getMonoType = \(ann, i) -> case M.lookup (NumberedVar i) subs of
        Just a -> a
        Nothing -> MTVar ann (NumberedVar i)
  pure $ fmap getMonoType holes

getUnknown :: Annotation -> TcMonad MonoType
getUnknown ann = MTVar ann . NumberedVar <$> getNextUniVar
