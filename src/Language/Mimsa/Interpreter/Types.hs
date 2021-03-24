module Language.Mimsa.Interpreter.Types
  ( App,
    InterpretState (..),
    readScope,
    nextVariable,
    addToScope,
    askForSwaps,
    addOperator,
    copySwap,
    findOperator,
    incrementApplyCount,
  )
where

import Control.Monad.Except
import Control.Monad.Trans.State.Lazy
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.Swaps

type App ann =
  StateT
    (InterpretState ann)
    (Either (InterpreterError ann))

data InterpretState ann = InterpretState
  { isVarNum :: Int,
    isScope :: Scope ann,
    isInfix :: Map InfixOp Variable,
    -- number of applications we have done for timeout
    isApplyCount :: Int,
    isSwaps :: Swaps
  }
  deriving (Eq, Ord, Show)

-- infix operators

addOperator :: InfixOp -> Variable -> App ann ()
addOperator infixOp expr = do
  modify (\is -> is {isInfix = isInfix is <> M.singleton infixOp expr})

findOperator :: InfixOp -> App ann (Maybe Variable)
findOperator infixOp = do
  ops <- gets isInfix
  pure (M.lookup infixOp ops)

-- variable numbers

nextInt :: App ann Int
nextInt = do
  int' <- gets isVarNum
  modify (\is -> is {isVarNum = 1 + isVarNum is})
  pure int'

nextVariable :: App ann Variable
nextVariable = NumberedVar <$> nextInt

-- scope

readScope :: App ann (Scope ann)
readScope = gets isScope

addToScope :: (Eq ann, Monoid ann) => Scope ann -> App ann ()
addToScope scope' =
  case foundALoop scope' of
    Nothing ->
      modify
        ( \is ->
            is
              { isVarNum = 1 + isVarNum is,
                isScope = scope' <> isScope is
              }
        )
    Just k -> throwError $ SelfReferencingBinding k
  where
    foundALoop (Scope newScope) =
      fmap fst . listToMaybe . M.toList . M.filterWithKey (\k a -> MyVar mempty k == a) $ newScope

-- infinity protection

-- number of function applications before we fail
maxCount :: Int
maxCount = 100000

incrementApplyCount :: App ann ()
incrementApplyCount = do
  appCount <- gets isApplyCount
  if appCount < maxCount
    then modify (\is -> is {isApplyCount = appCount + 1})
    else throwError MaximumCallSizeReached

-- track original names of numbered vars for pretty printing later

askForSwaps :: App ann Swaps
askForSwaps = gets isSwaps

addSwap :: Variable -> Name -> App ann ()
addSwap v n = modify (\is -> is {isSwaps = isSwaps is <> M.singleton v n})

findSwap :: Variable -> App ann (Maybe Name)
findSwap v =
  M.lookup v <$> askForSwaps

copySwap :: Variable -> Variable -> App ann ()
copySwap old new = do
  name <- findSwap old
  case name of
    Nothing -> pure ()
    Just foundSwap -> do
      addSwap new foundSwap
