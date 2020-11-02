module Language.Mimsa.Interpreter.Types (App, readScope, nextVariable, addToScope, askForSwaps) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor (bimap, first)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Language.Mimsa.Types

type App ann =
  StateT (Int, Scope ann)
    (ReaderT Swaps (Either (InterpreterError ann)))

readScope :: App ann (Scope ann)
readScope = gets snd

nextInt :: App ann Int
nextInt = do
  int' <- gets fst
  modify $ first (1 +)
  pure int'

nextVariable :: App ann Variable
nextVariable = NumberedVar <$> nextInt

addToScope :: (Eq ann, Monoid ann) => Scope ann -> App ann ()
addToScope scope' =
  case foundALoop scope' of
    Nothing -> modify $ bimap (1 +) (scope' <>)
    Just k -> throwError $ SelfReferencingBinding k
  where
    foundALoop (Scope newScope) =
      fmap fst . listToMaybe . M.toList . M.filterWithKey (\k a -> MyVar mempty k == a) $ newScope

askForSwaps :: App ann Swaps
askForSwaps = ask
