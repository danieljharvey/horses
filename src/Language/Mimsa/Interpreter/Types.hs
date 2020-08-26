module Language.Mimsa.Interpreter.Types (App, readScope, nextVariable, addToScope, askForSwaps) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor (bimap, first)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Language.Mimsa.Logging
import Language.Mimsa.Types

type App = StateT (Int, Scope) (ReaderT Swaps (ExceptT InterpreterError IO))

readScope :: App Scope
readScope = gets snd

nextInt :: App Int
nextInt = do
  int' <- gets fst
  modify $ first (1 +)
  pure int'

nextVariable :: App Variable
nextVariable = NumberedVar <$> nextInt

addToScope :: Scope -> App ()
addToScope scope' =
  case foundALoop (debugPretty "new scope" scope') of
    Nothing -> modify $ bimap (1 +) (scope' <>)
    Just k -> throwError $ SelfReferencingBinding k
  where
    foundALoop (Scope newScope) =
      fmap fst . listToMaybe . M.toList . M.filterWithKey (\k a -> MyVar k == a) $ newScope

askForSwaps :: App Swaps
askForSwaps = ask
